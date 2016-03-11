namespace Speed.Gui

open System
open System.Windows.Forms

module Program =
  [<EntryPoint; STAThread>]
  let main argv =
    Application.Run(new MainForm())

    // exit code
    0
