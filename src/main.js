import { Elm } from "./Main.elm";

const tasksFileName = './tasks.json'

try {
  window.Neutralino.init()
} catch (err) {
  console.error(err)
}

try {
  const url = "https://raw.githubusercontent.com/ypyl/rotate/master/update.json"
  const manifest = await Neutralino.updater.checkForUpdates(url)

  if (manifest.version != NL_APPVERSION) {
      await Neutralino.updater.install()
      await Neutralino.app.restartProcess()
  }
}
catch (err) {
  console.log(err)
}

let tasks = [];
try {
  const data = await Neutralino.filesystem.readFile(tasksFileName)
  const parsedModel = JSON.parse(data)
  tasks = parsedModel.tasks
} catch (err) {
  console.log(err)
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
    console.log(err)
  }
});
