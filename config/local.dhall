{

  hostname = "localhost",
  port     = +8080,
  autosave = "./autosave.json~"

  taskTree = {
    dependencies : List { _1 : Text, _2 : Text }
      = [ { _1 = "Full Gizmo", _2 = "Widgit A" },
          { _1 = "Full Gizmo", _2 = "Carriage" },
          { _1 = "Carriage"  , _2 = "Widgit B" },
          { _1 = "Carriage"  , _2 = "Sprog"    },
          { _1 = "Full Gizmo', _2 = "Pin"      }
        ],
    workers : List Text
      = [ "Greg", "Robert", "James", "Kenny" ]
  }

}