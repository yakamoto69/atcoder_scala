function copyExamples() {
  var $e = $('span.lang-ja pre').filter((i, e) => e.id.startsWith('pre-sample'));
  var txt = $e.toArray().map(e => e.textContent).reduce((a, b) => a + '\n' + b);
  navigator.clipboard.writeText(txt);
}

var $btn = $('<span class="btn btn-default btn-sm btn-copy">Copy All</span>');
$btn.on('click', e => copyExamples());
$btn.insertAfter($('div.io-style + hr').eq(0));