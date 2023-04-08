import { Elm } from "./Main.elm";

if (typeof NL_APPVERSION === "undefined") {
  initElm([])
} else {
  const tasksFileName = './tasks.json'

  try {
    window.Neutralino.init()
  } catch (err) {
    console.error(err)
  }

  // try {
  //   const url = "https://raw.githubusercontent.com/ypyl/rotate/master/update.json"
  //   const manifest = await Neutralino.updater.checkForUpdates(url)
  //   console.log(`Current version is ${NL_APPVERSION}`);
  //   console.log(`Latest version is ${manifest.version}`);
  //   if (manifest.version != NL_APPVERSION) {
  //     console.log(`Updating app to ${manifest.version}`);
  //     await Neutralino.updater.install()
  //     await Neutralino.app.restartProcess()
  //   }
  // } catch (err) {
  //   console.log(err)
  // }

  let tasks = [];
  try {
    const data = await Neutralino.filesystem.readFile(tasksFileName)
    const parsedModel = JSON.parse(data)
    tasks = parsedModel.tasks
  } catch (err) {
    console.log(err)
  }


  const app = initElm(tasks);

  app.ports.setState.subscribe(async function (state) {
    try {
      await Neutralino.filesystem.writeFile(tasksFileName, JSON.stringify(state, null, 2))
    } catch (err) {
      console.log(err)
    }
  });
}

function initElm(tasks) {
  var model = {
    windowWidth: window.innerWidth,
    windowHeight: window.innerHeight,
    tasks: tasks
  };

  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: model,
  });
  return app;
}
