import { Elm } from "./Main.elm";

try {
  window.Neutralino.init();
} catch {}

var model = {
  windowWidth: window.innerWidth,
  windowHeight: window.innerHeight,
  tasks: [
    {
      value: "single-active",
      date: "2023-03-14",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: "active",
    },
    {
      value: "slide-active",
      date: "2023-03-09",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: { endDate: "2023-03-28", status: "active" },
    },
    {
      value: "slide-failed",
      date: "2023-03-09",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: { endDate: "2023-03-15", status: "active" },
    },
    {
      value: "cron-active",
      date: "2022-03-09",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: { cron: "* * *", endDate: "2023-09-14", cases: [] },
    },
    {
      value: "single-done",
      date: "2023-03-14",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: "done",
    },
    {
      value: "slide-done",
      date: "2023-03-09",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: { endDate: "2023-03-28", status: "done" },
    },
    {
      value: "single-cancel",
      date: "2023-03-14",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: "cancel",
    },
    {
      value: "slide-cancel",
      date: "2023-03-09",
      createdDate: "2023-03-14",
      editDate: "2023-03-14",
      taskType: { endDate: "2023-03-28", status: "cancel" },
    },
  ],
};

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: model,
});

app.ports.setState.subscribe(function (state) {
  console.log(JSON.stringify(state))
});
