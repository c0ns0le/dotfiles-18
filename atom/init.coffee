# from dotfiles

os = require "os"


# atom.workspace.observeTextEditors (editor) ->
#   editor.onDidSave ->
#     fileSaved = editor.getPath()
#     if fileSaved.indexOf("init.coffee") > -1
#       atom.commands.dispatch "body", "window:reload"

# .platform-win32
atom.commands.add "body",
  "ryan:open-dotfiles": (event) ->
    atom.open
      newWindow: true
      # safeMode: true
      pathsToOpen: [os.homedir() + "/dotfiles/atom/styles.less"]
  "ryan:open-init-coffee": (event) ->
    atom.workspace.open(os.homedir() + "/dotfiles/atom/init.coffee")
  "ryan:open-styles": (event) ->
    atom.workspace.open(os.homedir() + "/dotfiles/atom/styles.less")
  "ryan:open-config": (event) ->
    atom.workspace.open(os.homedir() + "/dotfiles/atom/config.cson")
  "ryan:open-keymap": (event) ->
    atom.workspace.open(os.homedir() + "/dotfiles/atom/keymap.cson")

# { infoFromUri } = require "C:\\Users\\RYOLSON\\.atom\\packages\\atom-ide-ui\\modules\\atom-ide-ui\\pkg\\atom-ide-terminal\\lib\\nuclide-terminal-uri"
# ideterminal = require "C:\\Users\\RYOLSON\\.atom\\packages\\atom-ide-ui\\modules\\atom-ide-ui\\pkg\\atom-ide-terminal"

# getTerminal = () ->
#   panes = atom.workspace.getPanes()
#   for pane in panes
#     items = pane.getItems()
#     terminals = items.filter (item) -> item.getURI().startsWith "atom://nuclide-terminal-view"
#
#     if terminals.length > 0
#       terminal = terminals[0]
#
#       return [pane, terminal]
#
#   return undefined
#
# atom.commands.add "atom-text-editor",
#   "atom-ide-terminal:new-terminal-or-current": (event) ->
#     existingTerminal = getTerminal()
#
#     if existingTerminal
#       [pane, terminal] = existingTerminal
#       pane.activate()
#       pane.activateItem terminal
#     else
#       # console.log infoFromUri atom.workspace.getActivePaneItem().getURI()
#       # dir = atom.workspace.getActivePaneItem().getURI().split("\\").slice(0, -1).join("\\")
#       # ideterminal.provideTerminal().open({cwd: os.homedir()})
#       target = atom.views.getView atom.workspace
#       atom.commands.dispatch(target, 'atom-ide-terminal:new-terminal')
#
# atom.commands.onDidDispatch (event) ->
#   console.log "event dispatched", event
