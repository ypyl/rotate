import { Elm } from "./Main.elm";

const tasksFileName = './tasks.json'

try {
  window.Neutralino.init();
} catch (err) {
  console.error(err)
}

let tasks = [];
try {
  const data = await Neutralino.filesystem.readFile(tasksFileName)
  const parsedModel = JSON.parse(data)
  tasks = parsedModel.tasks
} catch (err) {
  console.error(err)
}


var model = {
  windowWidth: window.innerWidth,
  windowHeight: window.innerHeight,
  tasks: tasks
};

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: model,
});

app.ports.setState.subscribe(async function (state) {
  try {
    await Neutralino.filesystem.writeFile(tasksFileName, JSON.stringify(state, null, 2))
  } catch (err) {
    console.error(err)

  }
});
