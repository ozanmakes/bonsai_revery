let () =
  Revery.App.initConsole ();
  Timber.App.enable ();
  Timber.App.setLevel Timber.Level.perf;

  Bonsai_revery.Start.start_standalone Todo_mvc.app ~initial_input:()
