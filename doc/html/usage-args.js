/**
 * Argument linking for dynamic R help pages using Prism.js.
 *
 * Scans the Usage code block, parses top-level function signatures
 * via Prism tokens, and links top-level arguments to their entries in
 * the Arguments section. The actual linking is done by three Prism hooks;
 *
 * 'before-highlight' : identifies whether it's a Usage block (and not Examples)
 *
 * 'after-tokenize' : identifies and flags arguments
 *
 * 'wrap' : creates and inserts appropriate <a href=... > entries
 *
 * Operators such as $, *, %*% and [[ indexing are not processed as
 * they would add unnecessary complication with limited benefit
 * 
 */
(function () {

    // Collect all argument IDs documented in the Arguments section
    function getDocumentedArgIds() {
        const ids = new Set();
        const argsHeading = document.getElementById('_sec_arguments');
        if (argsHeading) {
            let el = argsHeading.nextElementSibling;
            if (el) el.querySelectorAll('[id]').forEach(e => ids.add(e.id));
        }
        return ids;
    }

    // Check if an element is inside the Usage section
    function isUsageElement(elem) {
        const usageHeading = document.getElementById('_sec_usage');
        if (!usageHeading || !elem) return false;
        return usageHeading.nextElementSibling && usageHeading.nextElementSibling.contains(elem);
    }

    let isCurrentElementUsage = false;

    if (typeof Prism !== 'undefined' && Prism.hooks) {

        // 1. Identify when Prism is about to highlight the Usage code block
        Prism.hooks.add('before-highlight', function (env) {
            isCurrentElementUsage = isUsageElement(env.element);
        });

        // 2. Intercept Prism's tokens immediately after Prism.tokenize() runs
        Prism.hooks.add('after-tokenize', function (env) {
            if (!isCurrentElementUsage) return;
            isCurrentElementUsage = false; // reset for next block

            const validArgIds = getDocumentedArgIds();
            if (validArgIds.size === 0) return;

	    // arguments are strings, but not all strings are
	    // arguments. To identify arguments, we need to keep track
	    // of context... usually after '(', '[', or ',', but only
	    // if at top-level. Special case: replacement
	    // functions. No attempt has been made to handle operators
	    // and '[[' (which is parsed as two separate [-s by Prism).
            const tokens = env.tokens;
            let depth = 0;
            let inTopLevelCall = false;  // whether we are at the top level inside a function call
            let expectingArg = false;    // whether we expect next token to be an argument
            let seenEquals = false;      // whether we are after = (indicating default value)
            let expectingReplacementVal = false; // whether we are after <- 

	    // FIXME: linkArg() can be simplified if Prism's R grammar
            // marks up variable bindings as such instead of keeping
            // them untokenized. This is easy, but better to do it
            // upstream than in our local copy.
            function linkArg(idx, name) {
                if (!validArgIds.has(name)) return;
                const tok = tokens[idx];
                if (typeof tok === 'string') {
                    const regex = new RegExp(`(^|[^a-zA-Z0-9._])${name.replace(/[.*+?^${}()|[\\]]/g, '\\$&')}([^a-zA-Z0-9._]|$)`);
                    const m = tok.match(regex);
                    if (m) {
                        const matchIndex = tok.search(regex);
                        const prefix = tok.slice(0, matchIndex + m[1].length);
                        const suffix = tok.slice(matchIndex + m[1].length + name.length);
                        // Subdivide string into [prefix, Token('arg-link', name), suffix].
                        tokens[idx] = [prefix, new Prism.Token('arg-link', name), suffix];
                    }
                } else if (tok && tok.type === 'ellipsis') {
                    tokens[idx] = new Prism.Token('arg-link', '...', 'ellipsis');
                }
            }

            for (let i = 0; i < tokens.length; i++) {
                const tok = tokens[i];
                const isString = typeof tok === 'string';
                const content = isString ? tok : tok.content;
                const type = isString ? 'text' : tok.type;

                if (type === 'comment') continue;

                if (type === 'punctuation') {
                    // Entering bracket / parenthesis / brace
                    if (content === '(' || content === '[' || content === '{') {
                        depth++;
                        if (depth === 1 && (content === '(' || content === '[')) {

                            // Check if preceded by a function
                            // identifier. This is redundant in most
                            // cases, but could be a useful protection
                            // against non-standard non-function
                            // Usage.

                            let j = i - 1;
                            while (j >= 0 && typeof tokens[j] === 'string' && tokens[j].trim() === '') j--;
                            if (j >= 0) {
                                const prev = typeof tokens[j] === 'string' ? tokens[j].trim() : tokens[j].content;
                                if (/[a-zA-Z0-9._`]$/.test(prev)) {
                                    inTopLevelCall = true;
                                    expectingArg = true; // expect next token to be an argument.
                                    seenEquals = false;
                                    expectingReplacementVal = false;
                                }
                            }

                        }
                        continue;
                    }

                    // Exiting bracket / parenthesis / brace
                    if (content === ')' || content === ']' || content === '}') {
                        if (depth === 1 && content === ')' && inTopLevelCall) {
                            inTopLevelCall = false;
                        }
                        depth--;
                        continue;
                    }

                    // Argument separator at top level of function; expect next token to be an argument
                    if (depth === 1 && inTopLevelCall && content === ',') {
                        expectingArg = true;
                        seenEquals = false;
                        continue;
                    }
                }

                // Replacement function rhs: fun(...) <- value
                if (depth === 0 && !inTopLevelCall) {
                    if (type === 'operator' && (content === '<-' || content === '<<-')) {
                        expectingReplacementVal = true;
                        continue;
                    }
                    if (expectingReplacementVal) {
                        if (type === 'text') {
                            const m = content.match(/([a-zA-Z.][a-zA-Z0-9._]*|`[^`]+`)/);
                            if (m) {
                                const valName = m[1].replace(/`/g, '');
                                if (validArgIds.has(valName)) {
                                    linkArg(i, valName);
                                }
                                expectingReplacementVal = false;
                            }
                        }
                        continue;
                    }
                }

                // Inside top-level arguments
                if (depth === 1 && inTopLevelCall) {
                    // Default value assignment
                    if (type === 'operator' && content === '=') {
                        seenEquals = true;
                        expectingArg = false;
                        continue;
                    }

                    // Argument name before '=': link to entry in Arguments section
                    if (expectingArg && !seenEquals) {
                        if (type === 'ellipsis') {
			    linkArg(i, '...');
                            expectingArg = false;
                        } else if (type === 'text') {
                            const m = content.match(/([a-zA-Z.][a-zA-Z0-9._]*|`[^`]+`)/);
                            if (m) {
				linkArg(i, m[1].replace(/`/g, ''));
                                expectingArg = false;
                            }
                        }
                    }
                }
            }
        });

        // 3. Render 'arg-link' tokens as <a> links during Token.stringify()
        Prism.hooks.add('wrap', function (env) {
            if (env.type === 'arg-link') {
                env.tag = 'a';
                env.attributes.href = '#' + encodeURIComponent(env.content);
            }
        });
    }

    // Fallback: If Usage was already highlighted before this script loaded, re-highlight it once
    function init() {
        const usageHeading = document.getElementById('_sec_usage');
        if (usageHeading) {
            const code = usageHeading.nextElementSibling.querySelector('code');
            if (code && !code.querySelector('a.token.arg-link')) {
                if (typeof Prism !== 'undefined') {
                    Prism.highlightElement(code);
                }
            }
        }
    }

    if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', init);
    } else {
        init();
    }

})();
