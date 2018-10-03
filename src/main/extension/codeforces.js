function copyExamples() {
  var samples = $('div.sample-test pre').toArray().map(e => $(e).html().replace(/<br>|<BR>/g, '\n'));
  var txt = samples.reduce((a, b) => a.trim() + '\n\n' + b.trim());
  navigator.clipboard.writeText(txt);
}

var $btn = $('<div title="Copy All" class="input-output-copier">Copy All</div>');
$btn.on('click', e => copyExamples());
$('div.sample-tests div.section-title').append($btn);