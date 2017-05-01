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

consumeService = (packageName, providerName, fn) ->
  if atom.packages.isPackageActive(packageName)
    pack = atom.packages.getActivePackage(packageName)
    fn(pack.mainModule[providerName]())
  else
    disposable = atom.packages.onDidActivatePackage (pack) ->
      if pack.name is packageName
        disposable.dispose()
        fn(pack.mainModule[providerName]())

consumeService 'vim-mode-plus', 'provideVimModePlus', (service) ->
  {Base} = service

  Delete = Base.getClass('Delete')
  class DeleteWithBackholeRegister extends Delete
    @commandPrefix: 'vim-mode-plus-user'
    @registerCommand()
    execute: ->
      @vimState.register.name = "_"
      super

  class InsertNewlineAtPoint extends Base.getClass('Operator')
    @commandPrefix: 'vim-mode-plus-user'
    @registerCommand()
    requireTarget: false
    execute: ->
      @editor.insertText("\n")
