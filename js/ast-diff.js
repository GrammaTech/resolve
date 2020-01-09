renderDiff = () => {
  var diff = JSON.parse(document.getElementById('diff').innerHTML)
  var page = document.getElementById('page')
  page.innerHTML = diff
}
