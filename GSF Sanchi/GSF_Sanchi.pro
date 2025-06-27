;+
; :Description:
;    Describe the procedure.
;     基于Landsat-8 OLI影像数据，快速提取三七种植区域面积
;
;
; :Keywords:
;    INPUT_RASTER 输入影像
;    SpecLibFile_URI 输入端元数据（光谱端元包括：['sanqi','Veg','Soil']）
;    SanChi_Raster 输出三七种植区域结果影像
;    SanChi_URI
;
; :Author: 东华理工大学  Xie_RS_GIS小组
; :Email:  xiexj@ecut.edu.cn
;-
pro GSF_Sanchi,$
  INPUT_RASTER=INPUT_RASTER,$
  DEM_RASTER = DEM_RASTER,$
  Urban_RASTER=Urban_RASTER,$
  Apply_QUAC = Apply_QUAC,$
  NIR_BandNum = NIR_BandNum, $
  Epochs_Denoising = Epochs_Denoising ,$
  Threshold_SNRI = Threshold_SNRI ,$
  Threshold_Max = Threshold_Max ,$
  Threshold_DEM = Threshold_DEM,$
  Threshold_Slope = Threshold_Slope,$
  INPUT_FILE_SLI=INPUT_FILE_SLI,$
  KERNEL_SIZE = KERNEL_SIZE, $
  Class_Raster = Class_Raster, $
  SanChi_Raster = SanChi_Raster,$
  SanChi_URI = SanChi_URI,$
  Statistics_URI=Statistics_URI

  compile_opt idl2, hidden
  on_error, 2

  ;Get ENVI session
  e = ENVI(/CURRENT)
;  DataColl = e.Data
;  DataColl.add,INPUT_RASTER

  abortable = Enviabortable()
  ;Get the Broadcast Channel ;进度条
  oChannel = e.Getbroadcastchannel()

  oStartMessage = Envistartmessage('Sanchi Extraction', abortable)
  oChannel.Broadcast, oStartMessage
  If (abortable.Abort_requested) Then Return

  ;;----------------------------------------------------------------------
  ;;                          ------ 0 数据读取与输入配置  ------
  ;;----------------------------------------------------------------------
  if isa(INPUT_RASTER.SPATIALREF,$
    'ENVIStandardRasterSpatialRef') then begin
    PixelSize = INPUT_RASTER.SPATIALREF.PIXEL_SIZE/1000.0
    Unit_area = '(km^2)'
  endif else begin
    PixelSize = [1.0,1.0]
    Unit_area = '(Number of pixels)'
  endelse

  ;;获取原影像元数据信息
  metadata = INPUT_RASTER.METADATA
  SPATIALREF=input_raster.SPATIALREF
  nBands = input_raster.NBands
  Ncolumns=input_raster.Ncolumns & Nrows=input_raster.Nrows
  nir_index = NIR_BandNum - 1
  green_index = nir_index-2

  Threshold_SNRI = float(strsplit(Threshold_SNRI,' :：,， ',/EXTRACT))
  
  ;;----------------------------------------------------------------------
  ;;                          ------ 1 图像预处理 ------
  ;;----------------------------------------------------------------------
  percentComplete = 0b
  If Apply_QUAC Then Begin
    percentComplete = percentComplete + 2b
    oProgressMessage = Enviprogressmessage( $
      'Radiation correcting',percentComplete, abortable)
    oChannel.Broadcast, oProgressMessage

    ; 辐射定标
    CaliTask = Envitask('RadiometricCalibration')
    CaliTask.Input_raster = INPUT_RASTER ; Bands 1-7
    CaliTask.Output_data_type = 'Double'
    CaliTask.Execute

    ; 快速大气校正
    QUACTask = Envitask('QUAC')
    QUACTask.Input_raster = CaliTask.Output_raster
    QUACTask.Sensor = 'Landsat TM/ETM/OLI'
    QUACTask.Execute
    INPUT_RASTER = QUACTask.Output_raster
  Endif

  if KEYWORD_SET(DEM_RASTER) then begin
    percentComplete = percentComplete + 6b
    oProgressMessage = Enviprogressmessage($
      'Terrain modeling',percentComplete, abortable)
    oChannel.Broadcast, oProgressMessage

    PixelSize_DEM = DEM_RASTER.SPATIALREF.PIXEL_SIZE/1000.0
    if PixelSize_DEM[0] ne PixelSize[0] then begin
      DEM_RASTER_resize = ENVIResampleRaster(DEM_RASTER, $
        PIXEL_SCALE=1.0*PixelSize/PixelSize_DEM)
      ; Define the spatial range of the subRect
      subRect = [1, 1, INPUT_RASTER.nSamples, INPUT_RASTER.nLines]
      SpatialRef = INPUT_RASTER.SPATIALREF
      ; Convert file coordinates to map coordinates
      SpatialRef.ConvertFileToMap, subRect[0], subRect[1], ULx, ULy
      SpatialRef.ConvertFileToMap, subRect[0], subRect[3], LLx, LLy
      SpatialRef.ConvertFileToMap, subRect[2], subRect[1], URx, URy
      SpatialRef.ConvertFileToMap, subRect[2], subRect[3], LRx, LRy
      geoRect = [ LLx, LLy, URx, URy ]
      ; Get the task from the catalog of ENVITasks
      Task = ENVITask('GeographicSubsetRaster')
      Task.INPUT_RASTER = DEM_RASTER_resize
      Task.SUB_RECT = geoRect
      Task.Execute
      DEM_RASTER = Task.OUTPUT_RASTER
    endif
    ;; 计算坡度
    Task = ENVITask('TopographicModeling')
    Task.INPUT_RASTER = DEM_RASTER
    Task.KERNEL_SIZE = 3
    Task.PRODUCTS = ["Slope"]
    Task.Execute
    Slope_Raster = Task.OUTPUT_RASTER

    Mask_Terrain = ((DEM_RASTER.GetData(BAND=0) gt 0) and $
      (DEM_RASTER.GetData(BAND=0) Lt Threshold_DEM)) or $
      ((Slope_Raster.GetData(BAND=0) gt 0) and $
      (Slope_Raster.GetData(BAND=0) Lt Threshold_Slope))

  endif else Mask_Terrain = MAKE_ARRAY(Ncolumns,Nrows,value = 1b)

  if KEYWORD_SET(Urban_RASTER) then begin
    PixelSize_Urban = Urban_RASTER.SPATIALREF.PIXEL_SIZE/1000.0
    if PixelSize_Urban[0] ne PixelSize[0] then begin
      Urban_RASTER_resize = ENVIResampleRaster(Urban_RASTER, $
        PIXEL_SCALE=1.0*PixelSize/PixelSize_Urban)
      ; Define the spatial range of the subRect
      subRect = [1, 1, INPUT_RASTER.nSamples, INPUT_RASTER.nLines]
      SpatialRef = INPUT_RASTER.SPATIALREF
      ; Convert file coordinates to map coordinates
      SpatialRef.ConvertFileToMap, subRect[0], subRect[1], ULx, ULy
      SpatialRef.ConvertFileToMap, subRect[0], subRect[3], LLx, LLy
      SpatialRef.ConvertFileToMap, subRect[2], subRect[1], URx, URy
      SpatialRef.ConvertFileToMap, subRect[2], subRect[3], LRx, LRy
      geoRect = [ LLx, LLy, URx, URy ]
      ; Get the task from the catalog of ENVITasks
      Task = ENVITask('GeographicSubsetRaster')
      Task.INPUT_RASTER = Urban_RASTER_resize
      Task.SUB_RECT = geoRect
      Task.Execute
      Urban_RASTER = Task.OUTPUT_RASTER
    endif
    Mask_Urban = Urban_RASTER.GetData(BANDs=0,INTERLEAVE = 'bsq')
  endif

  ;;----------------------------------------------------------------------
  ;;               --------- 2 计算光谱反射率极值特征   ---------
  ;;----------------------------------------------------------------------
  percentComplete = percentComplete + 10b
  oProgressMessage = Enviprogressmessage( $
    'Spectral indies Extracting',percentComplete, abortable)
  oChannel.Broadcast, oProgressMessage

  data = Fltarr(Ncolumns,Nrows,2);;存储最大和SNRI特征
  ;Iterate through the data
  nlines = 1000000/Ncolumns ;每nlines计算
  For stepIndex =0, Nrows+nlines-1,nlines Do Begin
    ; Broadcast progress
    oProgressMessage.Percent = percentComplete + $
      (stepIndex+nlines)* 20.0/Nrows
    oChannel.Broadcast, oProgressMessage
    If (abortable.Abort_requested) Then BREAK

    MaxRows = stepIndex+nlines-1
    If MaxRows Gt Nrows-1 Then MaxRows = Nrows-1
    ;print,stepIndex,MaxRows
    SUB_RECT=[0,stepIndex,Ncolumns-1,MaxRows]
    ;print,input_raster.INTERLEAVE
    rasterTile=input_raster.Getdata(SUB_RECT=SUB_RECT,$
      INTERLEAVE = 'bsq')

    ;;如果输入数据为辐射亮度值，计算反射率数据0-1
    If mean(rasterTile,/NAN)gt 1.0 then begin
      IF (Metadata.Tags.HasValue('REFLECTANCE SCALE FACTOR')) THEN BEGIN
        scaleFactor = Metadata['REFLECTANCE SCALE FACTOR']
        rasterTile = rasterTile/(1.0*scaleFactor)
      endif else begin
        rasterTile = rasterTile/10000.0
      Endelse
    endif

    MIN_data = Min(rasterTile,Dimension=3,Max=Max_data,/NAN)
    ; SNRI 根据绿和NIR大小取正负
    SNRI_Data = (1.0*Max_data-MIN_data)/(1.0*Max_data+MIN_data)* $
      (-2.0*(rasterTile[*,*,green_index] gt rasterTile[*,*,nir_index])+1)
    data[*,stepIndex:MaxRows,*] = [[[SNRI_Data]],[[Max_data]]]
    ;output_Raster.Setdata,Out_Data,SUB_RECT=SUB_RECT
    If stepIndex+nlines Gt Nrows-1 Then Break
  Endfor

  IndexRaster = Enviraster(data,$
    SPATIALREF=SPATIALREF,NBANDS=2)
  IndexRaster.Save
  IndexRaster.Metadata.UpdateItem,'band names',['SNRI','Max']
  IndexRaster.WriteMetadata

  ;;----------------------------------------------------------------------
  ;;      ----------- 3 有符极差指数SNRI光谱指数决策分类 -----------
  ;;----------------------------------------------------------------------
  percentComplete = percentComplete + 20b
  oProgressMessage = Enviprogressmessage( $
    'Decision classification processing',percentComplete, abortable)
  oChannel.Broadcast, oProgressMessage

  input_Data = input_raster.Getdata(INTERLEAVE = 'bsq')
  Mask_Background = max(input_Data,DIMENSION=3) gt 0 ;;黑色背景掩膜
  Mask_water = (data[*,*,0] lt Threshold_SNRI[0])* $
    (data[*,*,1] lt Threshold_Max*1.5)               ;; 计算水体掩膜
  Mask_built = (data[*,*,0] lt Threshold_SNRI[2])* $
    (data[*,*,1] ge Threshold_Max)*(Mask_water eq 0) ;; 计算建筑掩膜

  if N_ELEMENTS(Mask_Urban) lt 1 then begin
    percentComplete = percentComplete + 5b
    oProgressMessage = Enviprogressmessage( $
      'Mask built-up area', percentComplete, abortable)
    oChannel.Broadcast, oProgressMessage

    ;;利用CONVOL计算核邻域和，求得建筑核密度Mask_Urban
    Size_kernel = fix(sqrt(0.03/(PixelSize[0])^2))
    kernel = uintarr(Size_kernel,Size_kernel)+1b
    CONVOL_built = CONVOL(fix(Mask_built), kernel,/EDGE_TRUNCATE)
    Mask_Urban = CONVOL_built gt N_ELEMENTS(kernel)

    metadata = ENVIRasterMetadata()
    Metadata.AddItem,'Band NAMES','Built-up Mask'
    Mask_Raster = enviraster(Mask_Urban,$
      SPATIALREF=SPATIALREF,Metadata=Metadata)
    Mask_Raster.save
  endif

  Mask_Sanchi = (data[*,*,0] ge Threshold_SNRI[0])* $
    (data[*,*,0] lt Threshold_SNRI[1])* $
    (data[*,*,1] lt Threshold_Max)*(Mask_Urban eq 0)* $
    (Mask_water eq 0)*(Mask_built eq 0)           ;; 计算三七掩膜
  ClassData = Mask_water+ Mask_built*2+Mask_Sanchi*3
  ClassData = ClassData*Mask_Terrain*Mask_Background

  ;;----------------------------------------------------------------------
  ;;            ------------4 分类与后处理  --------------
  ;;----------------------------------------------------------------------

  ;;------------------------ (1)分类结果去噪  ----------------------------
  if Epochs_Denoising gt 0 then begin
    percentComplete = percentComplete + 10b
    oProgressMessage = Enviprogressmessage( $
      'Classification Denoising',percentComplete, abortable)
    oChannel.Broadcast, oProgressMessage

    Pixel1DXY = where(ClassData ge 1 and ClassData le 3)
    ClassData = Pixel_Denoising(ClassData,Pixel1DXY,Epochs_Denoising,$
      Connectivity=4,Denoising_Method = 'mode',Percent = 3/4.0)
  endif

  ;; ------------------------ (2)根据优势度优化分类结果 ------------------------
  
    ;;设置分类文件属性
  metadata = ENVIRasterMetadata()
  CLASS_NAMES = ['unclassied','Water','Buildings','Sanchi']
  CLASS_LOOKUP=[[!Color.White],[!Color.BLUE],$
    [!Color.Red],[!Color.magenta]]
  Metadata.AddItem,'CLASSES',4
  Metadata.AddItem,'CLASS LOOKUP',CLASS_LOOKUP
  Metadata.AddItem, 'CLASS NAMES', CLASS_NAMES
  Metadata.AddItem,'Band NAMES','DecTree Classification'
  Class_Raster = enviraster(ClassData,$
    SPATIALREF=SPATIALREF,Metadata=Metadata,$
    uri = e.GetTemporaryFilename())
  Class_Raster.save

  ;   建立水体缓冲区
  ; Run the buffer zonetask
  BufferZoneTask = ENVITask('BufferZone')
  BufferZoneTask.INPUT_RASTER = Class_Raster
  BufferZoneTask.CLASS_NAME = 'Water'
  BufferZoneTask.OUTPUT_DATA_TYPE = 'Float'
  BufferZoneTask.MAXIMUM_DISTANCE = 10
  BufferZoneTask.Execute

  ; 根据水体缓冲区过滤周边三七错分像素
  rasterArray=[Class_Raster,BufferZoneTask.OUTPUT_RASTER]
  plus_raster=ENVILayerStackRaster(rasterArray)
  ZoonRaster = ENVIPixelwiseBandMathRaster(plus_raster,'B1-(B1 eq 3)*(B2 ge 1)*(B2 lt 2)*3')

  class_task=ENVITask('EditRasterMetadata')
  class_task.INPUT_RASTER = ZoonRaster
  class_task.CLASS_NAMES=CLASS_NAMES
  class_task.classes =4
  class_task.CLASS_LOOKUP=CLASS_LOOKUP
  ; Run the task
  class_task.Execute
 
  ;; ------------------------ (4)平滑最终分类结果   ------------------------
  if KEYWORD_SET(KERNEL_SIZE) then begin
    SmoothTask = ENVITask('ClassificationSmoothing')
    SmoothTask.KERNEL_SIZE = KERNEL_SIZE
    SmoothTask.INPUT_RASTER = class_task.OUTPUT_RASTER
    SmoothTask.OUTPUT_RASTER_URI = output_raster_uri
    SmoothTask.Execute

    ; Run the aggregation task
    AggregationTask = ENVITask('ClassificationAggregation')
    AggregationTask.INPUT_RASTER = SmoothTask.OUTPUT_RASTER
    AggregationTask.MINIMUM_SIZE = KERNEL_SIZE^2
    AggregationTask.Execute

    Class_Raster = AggregationTask.OUTPUT_RASTER
  endif else $
    Class_Raster = class_task.OUTPUT_RASTER

  ;;---------------------------------------------------------------------
  ;;              -------5 混合像元区域中三七比例提取--------
  ;;---------------------------------------------------------------------
  ; （1）三七种植地缓冲区提取
  ; ----
  
  if KEYWORD_SET(INPUT_FILE_SLI) then begin
    percentComplete = percentComplete + 10b
    oProgressMessage = Enviprogressmessage('Spectral unmixing', $
      percentComplete, abortable)
    oChannel.Broadcast, oProgressMessage

    ;  建立三七缓冲区
    BufferZoneTask = ENVITask('BufferZone')
    BufferZoneTask.INPUT_RASTER = Class_Raster
    BufferZoneTask.CLASS_NAME = 'Sanchi'
    BufferZoneTask.OUTPUT_DATA_TYPE = 'Float'
    BufferZoneTask.MAXIMUM_DISTANCE = 20
    BufferZoneTask.Execute

    ;  提取三七缓冲区掩膜
    ZoonRaster =BufferZoneTask.OUTPUT_RASTER
    ZoonRaster = ENVIPixelwiseBandMathRaster($
      ZoonRaster,'(B1 ge 1) and (B1 lt 2)')
    MIXed_mask = ENVIDataValuesMaskRaster(ZoonRaster, [1,1],/INVERSE)
    ; ----
    ; （2）缓冲区混合像元分解得到三七比例
    ; ----
    LinearSpectralUnmixing,INPUT_RASTER,INPUT_FILE_SLI.uri,$
      MIXed_mask=MIXed_mask,Unmixing_result= Unmixing_result

    ;----
    ; (3) 叠加纯净像元区域与混合像元区域
    ;----
    plus_raster = ENVILayerStackRaster([Class_Raster,Unmixing_result])
    SanChi_Raster = ENVIPixelwiseBandMathRaster(plus_raster,$
      '(b1 eq 3)+(0>b2<1)',NAME = 'Sanchi')
  endif else $
    SanChi_Raster = ENVIPixelwiseBandMathRaster(Class_Raster,$
    '(b1 eq 3)',NAME = 'Sanchi')

  ;;--------------------------------------------------------------------
  ;;                  --------6 结果输出与面积统计-------
  ;;--------------------------------------------------------------------
  ;----
  ; (1) 保存三七提取结果
  ;----
  if KEYWORD_SET(SanChi_URI) then $
    SanChi_Raster.Export, SanChi_URI, 'ENVI'
  ;----
  ; (2)显示结果
  ;----
  ; Add the output to the Data Manager
  ; 
;  DataColl.Add, Class_Raster
;  DataColl.Add, SanChi_Raster
;  view = e.Getview()
;  layer = view.Createlayer(Class_Raster)

  oProgressMessage.Percent = 95
  oChannel.Broadcast, oProgressMessage
  ; Broadcast finish
  oFinishMessage = Envifinishmessage(abortable)
  oChannel.Broadcast, oFinishMessage

  ;----
  ; (3)统计整个研究区三七种植面积
  ;----
  SanChiData=SanChi_Raster.getdata(bands=0)
  SanChiArea=total(SanChiData,/DOUBLE)*(PixelSize[0]*PixelSize[1])
  Water_Raster = ENVIPixelwiseBandMathRaster(Class_Raster,'(b1 eq 1)')
  WaterData=Water_Raster.getdata(bands=0)
  WaterArea=total(WaterData,/DOUBLE)*(PixelSize[0]*PixelSize[1])
  Area =[WaterArea,SanChiArea]
  Classes =[1,2]

  ;; 绘制直方图
  b1 = BARPLOT(Classes+0.2, Area, index=0, NBARS=2, FILL_COLOR='gold', $
    YMINOR=0, YTITLE='Area'+Unit_area, XTITLE='Class Number', $
    TITLE='Area of Sanchi plantations')

  Text1 = TEXT(1-0.2,WaterArea,'Water: '+ $
    strtrim(WaterArea,2)+' km^2', /CURRENT, COLOR='blue', /DATA)
  Text2 = TEXT(2-0.2,SanChiArea,'Sanchi: '+ $
    strtrim(SanChiArea,2)+Unit_area, /CURRENT, COLOR='magenta', /DATA)

  ;;统计数据写入文本文件
  if KEYWORD_SET(Statistics_URI) then begin
    ;将统计结果写入本地文本文件
    ;outAscii=FILE_DIRNAME(SanChi_URI)+path_sep()+'Statistics.txt'
    openw,lun,Statistics_URI, /GET_LUN
    printf,lun,'三七种植区域提取影像： '
    printf,lun,SanChi_URI
    printf,lun,'-----------------------------------------------------------'
    printf,lun,'全区三七种植面积为'+Unit_area+':'
    printf,lun,SanChiArea,format='('+string(6)+'f20.4)'
    free_lun,lun
  endif

end
