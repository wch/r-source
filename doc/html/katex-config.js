
const macros = {
    "\\R": "\\textsf{R}",
    "\\mbox": "\\text",
    "\\code": "\\texttt"
};

function processMathHTML()
{
    var i, l = document.getElementsByClassName('reqn');
    var opts = { throwOnError: false, macros: macros };
    for (i = 0; i < l.length; i++) {
	katex.render(l[i].textContent, l[i], opts);
    }
    return;
}

