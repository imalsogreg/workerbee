{

  hostname = "localhost",
  port     = +8080,
  autosave = "./autosave.json~",

  taskTree = {
    dependencies
      = [ { _1 = "Full Gizmo", _2 = "Widgit A" },
          { _1 = "Full Gizmo", _2 = "Carriage" },
          { _1 = "Carriage"  , _2 = "Widgit B" },
          { _1 = "Carriage"  , _2 = "Sprog"    },
          { _1 = "Full Gizmo", _2 = "Pin"      }
        ] : List { _1 : Text, _2 : Text },
    workers
      = [ "Greg", "Robert", "James", "Kenny" ] : List Text
  }

}