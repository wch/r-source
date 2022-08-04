// Used in tools:::.make_KaTeX_checker().
function checkTex(s)
{
    var opts = { throwOnError: true, macros: macros };
    var out = { error: "", warnings: [] };
    try {
	console.warn = function(w) { out.warnings.push(w); };
	katex.renderToString(s, opts);
    } catch (e) {
	out.error = e.message;
    };
    return out;
}
