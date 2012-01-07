classdef DatasetAnnotationDataAdapter <  bioinfoservices.AnnotationDataAdapter

    methods
        
        function obj = DatasetAnnotationDataAdapter(ds,fields,searchableFields,efficientFields)
           obj.FieldNames = fields;
           obj.Data = ds;
           obj.NumberOfEntries = size(ds,1);
           obj.EfficientAccessFields = efficientFields;
           obj.StringSearchableFields = searchableFields;             
        end
        
        function data = getField(obj,field)
            data = obj.Data.(field);
        end
        
        function sobj = getSubset(obj,idx)
            idx = unique(idx);
            sobj = obj;
            sobj.Data = sobj.Data(idx,:);
            sobj.NumberOfEntries = numel(idx);
        end
        
        
        function uni = getDictionaryInSearchableStringField(obj,field)
            uni = unique(getField(obj,field));
        end        
        
    end
end