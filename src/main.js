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
      date: "2023-03-24",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      status: "active",
    },
    {
      value: "slide-active",
      startDate: "2023-03-09",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      endDate: "2023-03-28",
      status: "active",
    },
    {
      value: "slide-failed",
      startDate: "2023-03-09",
      createdDate: "2023-03-07",
      editDate: "2023-03-24",
      endDate: "2023-03-12",
      status: "active"
    },
    {
      value: "cron-active",
      startDate: "2022-03-09",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      cron: "* * *",
      endDate: "2023-09-14",
      cases: []
    },
    {
      value: "single-done",
      date: "2023-03-24",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      status: "done",
    },
    {
      value: "slide-done",
      startDate: "2023-03-09",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      endDate: "2023-03-28",
      status: { value: "done", date: "2023-03-23" },
    },
    {
      value: "single-cancel",
      date: "2023-03-24",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      status: "cancel",
    },
    {
      value: "slide-cancel",
      startDate: "2023-03-09",
      createdDate: "2023-03-24",
      editDate: "2023-03-24",
      endDate: "2023-03-28",
      status: { value: "cancel", date: "2023-03-24" },
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
