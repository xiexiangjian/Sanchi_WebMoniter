pro test_GSFExportToPNG
  COMPILE_OPT idl2

  ;Get ENVI session
  e = ENVI()

    taskfile = 'D:\Coding\IDL\Xie_IDL\GSF Assistant\GsfExportToPNG.task'
  
    task = ENVITask(taskfile)
    r = e.UI.SelectTaskParameters(task)
  
    IF r NE 'OK' THEN RETURN
  
    tic
    task.Execute
    toc

;  file = 'D:\Experiment\Input\Sanqi\2024实验\OLI_shilin_res2.dat'  
;  class_file = 'D:\Experiment\Input\Sanqi\2024实验\2018-km-decTree_class.dat'  
;  rgb_file = 'D:\Experiment\Input\Sanqi\2024实验\OLI_shilin.dat'
;  input_raster = enviurlraster(class_file)
;;  rgb_raster = enviurlraster(rgb_file)
;
;  GSFExportToPNG,$
;    ;    bands = [3,2,1],$
;    input_raster = input_raster,$
;;    rgb_raster = rgb_raster,$
;;    data_ignore_value = 0,$
;;    valid_data_range = [0.001,1.0],$
;;    linear_percent = 2;,$
;;    ColorTableName = 'CB-Purples' ,$ ;"Rainbow",
;    raster_pngfile = raster_pngfile
;
;;  raster =e.OpenRaster(raster_pngfile)
;;  view = e.GetView()
;;  layer = view.createlayer(raster)

end

pro;+
; :Description:
;    Describe the procedure.
;       用GSFExportToPNG这个ENVItask替代ENVI自带的ExportRasterToPNG，
;       用于输出背景透明png图像
;
;
; :Keywords:
;    bands
;    input_raster
;    rgb_raster 用于叠加RGB真彩色显示的栅格数据
;    data_ignore_value
;    linear_percent
;    output_dir
;    raster_pngfile
;    ColorTableName
;    valid_data_range
;
; :Author: DELL
;-
 GSFExportToPNG,bands = bands,$
  input_raster = input_raster,$
  rgb_raster = rgb_raster,$
  data_ignore_value = data_ignore_value,$
  linear_percent = linear_percent,$
  output_dir = output_dir,$
  raster_pngfile = raster_pngfile,$
  ColorTableName = ColorTableName,$
  valid_data_range = valid_data_range


  COMPILE_OPT idl2

  ;Get ENVI session
  e = ENVI(/CURRENT)

  if N_ELEMENTS(output_dir) eq 0 then begin
    pro_dir = FILE_DIRNAME(input_raster.URI)
    output_dir = FILEPATH('output', root_dir=pro_dir)
    file_mkdir, output_dir
  endif
  cd, output_dir

  if N_ELEMENTS(raster_pngfile) lt 1 then begin
    inFileBaseNname = FILE_BASENAME(input_raster.URI)
    FileName = STRMID(inFileBaseNname,0,$
      STRPOS(inFileBaseNname, '.'))
    raster_pngfile =output_dir + PATH_SEP()+ FileName+'.PNG'
  endif

  ;如果bands没有设置，根据nb设置默认值
  IF ~N_ELEMENTS(bands) THEN BEGIN
    CASE input_raster.nbands OF
      1: bands = [0]
      2: bands = [0]
      3: bands = [0,1,2]
      ELSE: bands = [3,2,1] ;;OLI/Sentinel-2
    ENDCASE
  ENDIF

  dimensions = [input_raster.NCOLUMNS,input_raster.NROWS]

  ;
  if input_raster.nbands lt 3 then begin
    IF KEYWORD_SET(ColorTableName) and $
      ~input_raster.METADATA.hasTag('CLASSES') THEN BEGIN
      ;对单波段进行彩色显示
      Task = ENVITASK('ColorSliceClassification')
      Task.INPUT_RASTER = input_raster
      Task.DATA_MINIMUM = valid_data_range[0]+0.0001
      Task.DATA_MAXIMUM = valid_data_range[1]
      Task.NUMBER_OF_RANGES = 10
      Task.COLOR_TABLE_NAME = ColorTableName
      Task.Execute
      Raster = Task.OUTPUT_RASTER
    endif else Raster = input_raster

    rgb_table = Raster.METADATA['CLASS LOOKUP'] ;获取颜色表
    ;    IMGdata = Raster.GetData(INTERLEAVE = 'bip')
    ;    WRITE_PNG,raster_pngfile,IMGdata,$
    ;      RGB_TABLE[*,0],RGB_TABLE[*,1],RGB_TABLE[*,2],TRANSPARENT=transp
  Endif else begin
    ;波段裁剪
    Raster = ENVISubsetRaster(input_raster, bands=bands)
    
    ;拉伸
    IF KEYWORD_SET(linear_percent) THEN begin
      Raster = ENVILinearPercentStretchRaster($
        Raster, percent=linear_percent)
    ENDIF else Raster = input_raster

  Endelse

  ;对多波段进行RGB彩色合成显示  
  if KEYWORD_SET(rgb_raster) then begin
    rgbraster = ENVILinearPercentStretchRaster($
      rgb_raster, percent = 2)
    IMGdata = rgbraster.GetData(bands=[3,2,1],INTERLEAVE = 'bip')
    RGB_img = image(IMGdata,/BUFFER,$
      DIMENSIONS = DIMENSIONS*(1.0/((DIMENSIONS[0]/600)>1)),$
      IMAGE_DIMENSIONS = DIMENSIONS,$
      MARGIN=[0.,0.,0.,0.], /ORDER,ASPECT_RATIO=0,$
      BACKGROUND_COLOR=[0, 0, 0],BACKGROUND_TRANSPARENCY=100)
    TRANSPARENCY = 50
  endif else TRANSPARENCY = 0

  IMGdata = Raster.GetData(INTERLEAVE = 'bip')
  out_png = image(IMGdata,/BUFFER,RGB_TABLE=RGB_TABLE,$
    DIMENSIONS = DIMENSIONS*(1.0/((DIMENSIONS[0]/600)>1)),$
    IMAGE_DIMENSIONS = DIMENSIONS,/OVERPLOT,$
    MARGIN=[0.,0.,0.,0.], /ORDER,ASPECT_RATIO=0,TRANSPARENCY=TRANSPARENCY,$
    BACKGROUND_COLOR=[0, 0, 0],BACKGROUND_TRANSPARENCY=100)
    
  out_png.save,raster_pngfile,$
    TRANSPARENT=[0, 0, 0],WIDTH=DIMENSIONS[0]<1200
end