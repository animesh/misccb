classdef BIFAnnotationDataAdapter <  bioinfoservices.AnnotationDataAdapter
    
    properties
        FieldBinnig
        NumericFields
    end
    
    methods
        
        function obj = BIFAnnotationDataAdapter(data,fields,searchableFields,efficientFields,fieldBinning,numericFields)
            obj.FieldNames = fields;
            obj.Data = data;
            obj.NumberOfEntries = data.NumEntries;
            obj.EfficientAccessFields = efficientFields;
            obj.StringSearchableFields = searchableFields;            
            obj.FieldBinnig = containers.Map(fields,fieldBinning);
            obj.NumericFields = numericFields;
        end
        
        function  data = getField(obj,field)
            if ismember(field,obj.NumericFields)
                formatStr = [repmat('%*s',1,obj.FieldBinnig(field)-1) '%d%*[^\n]'];
            else
                formatStr = [repmat('%*s',1,obj.FieldBinnig(field)-1) '%s%*[^\n]'];
            end
            try
                tmp = textscan(getEntryByIndex(obj.Data,1:obj.NumberOfEntries),formatStr,'delimiter','\t','ReturnOnError',0);
            catch ME %#ok<NASGU>
                bioinfoprivate.bioclserror(mfilename,'getField',...
                    'InvalidTableFormatInBIF',...
                    'Cannot extract field ''%s'' from the BioIndexedFile object.',field)
            end
            data = tmp{1};
            if (iscellstr(data) && all(cellfun(@isempty,data))) || (isnumeric(data) && all(isnan(data)))
                bioinfoprivate.bioclserror(mfilename,'getField',...
                    'InvalidTableFormatInBIF',...
                    'Cannot extract field ''%s'' from the BioIndexedFile object.',field)
            end
        end
        
        function sobj = getSubset(obj,idx)
            idx = unique(idx);
            sobj = obj;
            sobj.Data = getSubset(sobj.Data,idx);
            sobj.NumberOfEntries = sobj.Data.NumEntries;
        end
        
    end
end