;+
; :Description:
;    Describe the procedure.
;     基于线性混合像元分解混合区域中的田七种植面积
;
;
; :Keywords:
;    MIXed_mask 混合区域掩膜数据
;    SpecLibFile 输入端元数据（光谱端元包括：['sanqi','Veg','Soil']）
;    INPUT_RASTER 输入影像
;    Unmixing_result 解混结果（田七组份）
;
; :Author: 东华理工大学  Xie_RS_GIS小组
; :Email:  xiexj@ecut.edu.cn
;-
pro LinearSpectralUnmixing,INPUT_RASTER,SpecLibFile,$
  MIXed_mask=MIXed_mask,$
  Unmixing_result=Unmixing_result
  
  COMPILE_OPT IDL2
  envi = ENVI(/CURRENT)
  
  if KEYWORD_SET(MIXed_mask) then begin
    INPUT_RASTER = ENVIMaskRaster(INPUT_RASTER, MIXed_mask)
  endif 
  
  ;打开光谱数据库文件
  SpecLib= ENVISpectralLibrary(SpecLibFile)
  ;获取端元
  nEndmembers = N_Elements(SpecLib.SPECTRA_NAMES)
  endmembers = DblArr(INPUT_RASTER.NBands, nEndmembers, /NOZERO)
  FOR i=0, nEndmembers-1 DO BEGIN
    Spectrum=specLib.GetSpectrum(SpecLib.SPECTRA_NAMES[i])
    endmembers[*,i]=Spectrum.SPECTRUM
  ENDFOR
  
  ;运行线性混合分解task
  Unmixing_task = ENVITask('LinearSpectralUnmixing')
  ;定义输入文件
  Unmixing_task.INPUT_RASTER = INPUT_RASTER
  Unmixing_task.ENDMEMBERS = endmembers
  Unmixing_task.WEIGHT = 2.0
  ;定义输出文件
  ;Unmixing_task.OUTPUT_RASTER_URI = envi.GetTemporaryFilename()
  Unmixing_task.Execute

  ; Add metadata to raster for band names
  Unmixing_task.OUTPUT_RASTER.Metadata.UpdateItem,'band names',$
    [SpecLib.SPECTRA_NAMES,'error']
  ; Update the ENVI format *.hdr file with new metadata.
  Unmixing_task.OUTPUT_RASTER.WriteMetadata
  
  ; ----
  ;提取混合区域分解结果
  ; ----
  band_names = Unmixing_task.OUTPUT_RASTER.Metadata['band names']
  Unmixing_result=ENVISubsetRaster($
    Unmixing_task.OUTPUT_RASTER,BANDS=where(band_names eq 'Sanchi'))
end