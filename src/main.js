import { Elm } from './Main.elm'

try {
  window.Neutralino.init()
} catch {

}

var model =
  { windowWidth : window.innerWidth
  , windowHeight : window.innerHeight
  , tasks : []
  }

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: model
})

app.ports.setState.subscribe(function(state) {
  console.log(JSON.stringify(state))
});
