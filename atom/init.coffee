# from dotfiles

# Your init script
#
# Atom will evaluate this file each time a new window is opened. It is run
# after packages are loaded/activated and after the previous editor state
# has been restored.
#
# An example hack to log to the console when each text editor is saved.
#
# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     console.log "Saved! #{editor.getPath()}"

os = require "os"
{ infoFromUri } = require "C:\\Users\\RYOLSON\\.atom\\packages\\atom-ide-ui\\modules\\atom-ide-ui\\pkg\\atom-ide-terminal\\lib\\nuclide-terminal-uri"
ideterminal = require "C:\\Users\\RYOLSON\\.atom\\packages\\atom-ide-ui\\modules\\atom-ide-ui\\pkg\\atom-ide-terminal"

getTerminal = () ->
  panes = atom.workspace.getPanes()
  for pane in panes
    items = pane.getItems()
    terminals = items.filter (item) -> item.getURI().startsWith "atom://nuclide-terminal-view"

    if terminals.length > 0
      terminal = terminals[0]

      return [pane, terminal]

  return undefined

atom.commands.add "atom-text-editor",
  "atom-ide-terminal:new-terminal-or-current": (event) ->
    existingTerminal = getTerminal()

    if existingTerminal
      [pane, terminal] = existingTerminal
      pane.activate()
      pane.activateItem terminal
    else
      # console.log infoFromUri atom.workspace.getActivePaneItem().getURI()
      # dir = atom.workspace.getActivePaneItem().getURI().split("\\").slice(0, -1).join("\\")
      # ideterminal.provideTerminal().open({cwd: os.homedir()})
      target = atom.views.getView atom.workspace
      atom.commands.dispatch(target, 'atom-ide-terminal:new-terminal')

atom.commands.onDidDispatch (event) ->
  console.log "event dispatched", event

atom.workspace.observeTextEditors (editor) ->
  editor.onDidSave ->
    fileSaved = editor.getPath()
    if fileSaved.indexOf("init.coffee") > -1
      atom.commands.dispatch "body", "window:reload"
