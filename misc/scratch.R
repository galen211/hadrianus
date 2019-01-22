tabItem(tabName = "process_selector",
        selectInput("xes_file", "Choose a process file:",
                    list.files("data")
        ),
        actionButton("load_file", label = "Load Process", icon = icon("spinner", lib = "font-awesome")),
        helpText("Loading the selected file will reload the process data in the dashboard")
),


selectInput(
  "rank_dir",
  "Graph Orientation",
  choices =
    c(
      "Top-Bottom" = "TB",
      "Left-Right" = "LR",
      "Bottom-Top" = "BT",
      "Right-Left" = "RL"
    ),
  selected = "TB",
  width = "25%"
)