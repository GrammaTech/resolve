renderDiff = () => {
  var diff = JSON.parse(document.getElementById('diff').innerHTML)
  var page = document.getElementById('page')
  page.innerHTML += "\n"
  diff.forEach(item => {
    page.innerHTML += `<span class="${item[0]}">${item[1]}</span>`
  })
}
