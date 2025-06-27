; Add the extension to the toolbox. Called automatically on ENVI startup.
pro ENVI_GSF_Sanchi_extensions_init

  ; Set compile options
  compile_opt IDL2

  ; Get ENVI session
  e = ENVI(/CURRENT)

  ; Add the extension to a subfolder
  e.AddExtension, 'GSF Sanchi', 'ENVI_GSF_Sanchi', PATH=''
end

; ENVI Extension code. Called when the toolbox item is chosen.
pro ENVI_GSF_Sanchi

  ; Set compile options
  compile_opt IDL2

  ; General error handler
  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if OBJ_VALID(e) then $
      e.ReportError, 'ERROR: ' + !error_state.msg
    MESSAGE, /RESET
    return
  endif

  ;Get ENVI session
  e = ENVI(/CURRENT)

  ;******************************************
  ; Insert your ENVI Extension code here...
  ;******************************************
  
;  taskfile = 'D:\Coding\IDL\Xie_IDL\GSF Sanchi\GSFSanchi.task'
    
  task = ENVITask('GSFSanchi') ;taskfile)

  ;调用自定义ENVItask的动态UI
  r = e.UI.SelectTaskParameters(task)

  IF r NE 'OK' THEN RETURN

  tic

  ;执行自定义ENVItask
  task.Execute

  toc
end
