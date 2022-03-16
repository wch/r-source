
MathJax = {
  tex: {
    macros: {
      R: "{\\textsf{R}}",
      code: ["\\texttt{#1}", 1]
    }
  },
  options: {
    renderActions: {
      find: [10, function (doc) {
        for (const node of document.querySelectorAll('code[class^="reqn"]')) {
          const math = new doc.options.MathItem(node.textContent, doc.inputJax[0], false);
          const text = document.createTextNode('');
          node.parentNode.replaceChild(text, node);
          math.start = {node: text, delim: '', n: 0};
          math.end = {node: text, delim: '', n: 0};
          doc.math.push(math);
        }
      }, '']
    }
  }
};
