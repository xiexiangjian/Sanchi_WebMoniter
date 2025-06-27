pro test_PreProcess
  compile_opt idl2

  e = ENVI(/CURRENT)

  taskfile = 'D:\Coding\IDL\Xie_IDL\Experiment\PreProcess.task'
  task = ENVITask(taskfile)

  ;调用自定义ENVItask的动态UI
  r = (e.UI).SelectTaskParameters(task)

  IF r NE 'OK' THEN RETURN

  tic

  ;执行自定义ENVItask
  task.Execute
  
;  View = e.GetView()
;  Layer = View.CreateLayer(task.OUTPUT_RASTER)

  toc


end


pro PreProcess,$
  Metadata_URI = Metadata_URI, $
  Apply_QUAC = Apply_QUAC,$
  INPUT_Vector  = INPUT_Vector, $
  OUTPUT_RASTER = OUTPUT_RASTER, $
  OUTPUT_URI = OUTPUT_URI

  compile_opt idl2

  If N_elements(Metadata_URI) Gt 0 Then Begin
    basename= file_basename(Metadata_URI[0])  ;文件名
    FileName = strmid(basename,0,strpos(basename,'.')) ;文件名不含拓展名
    LevelName = strmid(basename,5,2) ;产品级别如L1TP
    typename = strmid(basename,strpos(basename,'.')+1)   ;拓展名
  Endif else $
    MESSAGE, /IOERROR, "Metadata file is required for this function."

  e = ENVI(/CURRENT)  
  
  if (LevelName eq 'L1') and (typename eq 'txt') then begin
    OUTPUT_RASTER = ENVIURLRaster(Metadata_URI[0],DATASET_INDEX=0)
    If N_elements(INPUT_Vector) Gt 0 Then Begin
      Vector = e.OpenVector(INPUT_Vector)

      Task = ENVITask('CreateSubrectsFromVector')
      Task.INPUT_VECTOR = Vector
      Task.INPUT_RASTER = OUTPUT_RASTER
      Task.Execute

      Task2 = ENVITask('SubsetRaster')
      Task2.INPUT_RASTER = OUTPUT_RASTER
      Task2.Sub_Rect = Task.SUBRECTS
      Task2.Execute
      OUTPUT_RASTER = Task2.OUTPUT_RASTER
    Endif

    If Apply_QUAC Then Begin
      ; 辐射定标
      CaliTask = Envitask('RadiometricCalibration')
      CaliTask.Input_raster = OUTPUT_RASTER ; Bands 1-7
      CaliTask.Output_data_type = 'Double'
      CaliTask.Execute

      ; 快速大气校正
      QUACTask = Envitask('QUAC')
      QUACTask.Input_raster = CaliTask.Output_raster
      QUACTask.Sensor = 'Landsat TM/ETM/OLI'
      QUACTask.Execute
      OUTPUT_RASTER = QUACTask.Output_raster
    Endif
  endif else begin
    if (LevelName eq 'L2') and (typename eq 'json') then begin

      LandsatC2_Obj = LandsatC2(Metadata_URI[0])
      ;; Load Multispectral Imagery (加载多光谱影像)
      LandsatC2_Obj.load
      
      ;; Restore the image to the real surface reflectance (0-1)
      ;;   (scaling:将影像缩放还原为真实地表反射率(0-1))
      OUTPUT_RASTER = LandsatC2_Obj.Scaling()
      LandsatC2_Obj.Cleanup
      
      If N_elements(INPUT_Vector) Gt 0 Then Begin
        Vector = e.OpenVector(INPUT_Vector)
  
        Task = ENVITask('CreateSubrectsFromVector')
        Task.INPUT_VECTOR = Vector
        Task.INPUT_RASTER = OUTPUT_RASTER
        Task.Execute
  
        Task2 = ENVITask('SubsetRaster')
        Task2.INPUT_RASTER = OUTPUT_RASTER
        Task2.Sub_Rect = Task.SUBRECTS
        Task2.Execute
        OUTPUT_RASTER = Task2.OUTPUT_RASTER
      Endif      
           
    endif else $
      MESSAGE, /IOERROR, "Metadata file cannot read with Envi56."
  endelse
  
  If N_elements(INPUT_Vector) Gt 0 Then Begin
;    Vector = e.OpenVector(INPUT_Vector)
    Task = ENVITask('VectorMaskRaster')
    Task.DATA_IGNORE_VALUE = 0
    Task.INPUT_MASK_VECTOR = Vector
    Task.INPUT_RASTER = OUTPUT_RASTER
    Task.Execute
    OUTPUT_RASTER = Task.OUTPUT_RASTER
  Endif
  
  if N_ELEMENTS(OUTPUT_URI) Gt 0 and OUTPUT_URI ne '*' Then Begin
    OUTPUT_RASTER.export, OUTPUT_URI, 'ENVI',DATA_IGNORE_VALUE=0
  endif
end