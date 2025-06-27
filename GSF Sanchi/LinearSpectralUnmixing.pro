;+
; :Description:
;    Describe the procedure.
;     �������Ի����Ԫ�ֽ��������е�������ֲ���
;
;
; :Keywords:
;    MIXed_mask ���������Ĥ����
;    SpecLibFile �����Ԫ���ݣ����׶�Ԫ������['sanqi','Veg','Soil']��
;    INPUT_RASTER ����Ӱ��
;    Unmixing_result �������������ݣ�
;
; :Author: ��������ѧ  Xie_RS_GISС��
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
  
  ;�򿪹������ݿ��ļ�
  SpecLib= ENVISpectralLibrary(SpecLibFile)
  ;��ȡ��Ԫ
  nEndmembers = N_Elements(SpecLib.SPECTRA_NAMES)
  endmembers = DblArr(INPUT_RASTER.NBands, nEndmembers, /NOZERO)
  FOR i=0, nEndmembers-1 DO BEGIN
    Spectrum=specLib.GetSpectrum(SpecLib.SPECTRA_NAMES[i])
    endmembers[*,i]=Spectrum.SPECTRUM
  ENDFOR
  
  ;�������Ի�Ϸֽ�task
  Unmixing_task = ENVITask('LinearSpectralUnmixing')
  ;���������ļ�
  Unmixing_task.INPUT_RASTER = INPUT_RASTER
  Unmixing_task.ENDMEMBERS = endmembers
  Unmixing_task.WEIGHT = 2.0
  ;��������ļ�
  ;Unmixing_task.OUTPUT_RASTER_URI = envi.GetTemporaryFilename()
  Unmixing_task.Execute

  ; Add metadata to raster for band names
  Unmixing_task.OUTPUT_RASTER.Metadata.UpdateItem,'band names',$
    [SpecLib.SPECTRA_NAMES,'error']
  ; Update the ENVI format *.hdr file with new metadata.
  Unmixing_task.OUTPUT_RASTER.WriteMetadata
  
  ; ----
  ;��ȡ�������ֽ���
  ; ----
  band_names = Unmixing_task.OUTPUT_RASTER.Metadata['band names']
  Unmixing_result=ENVISubsetRaster($
    Unmixing_task.OUTPUT_RASTER,BANDS=where(band_names eq 'Sanchi'))
end