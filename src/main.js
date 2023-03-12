import { Elm } from './Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: [window.innerWidth, window.innerHeight]
})

try {
  window.Neutralino.init()
} catch {

}
