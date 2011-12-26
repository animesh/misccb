classdef StructAnnotationDataAdapter <  bioinfoservices.AnnotationDataAdapter

    properties
        NumericFields
    end
    
    methods
        
        function obj = StructAnnotationDataAdapter(data,fields,searchableFields,efficientFields,numericFields)
            infieldnames = fieldnames(data);
            if numel(infieldnames)~=numel(fields)
                data = rmfield(data,setdiff(infieldnames,fields));
                infieldnames = fieldnames(data);
            end
            if all(strcmp(infieldnames(:),fields(:)))
                obj.Data = data;
            else
                obj.Data = orderfields(data,fields);
            end
            obj.NumberOfEntries = numel(data);
            obj.FieldNames = fields;
            obj.EfficientAccessFields = efficientFields;
            obj.StringSearchableFields = searchableFields;
            if nargin==4
                numericFields = false(1,numel(fields));
                for i = 1:numel(fields)
                    field = fields{i};
                    if all(cellfun(@(x) isnumeric(x)&isscalar(x),{obj.Data.(field)}))
                       numericFields(i) = true;
                    end
                end
                obj.NumericFields = fields(numericFields);
            else
                obj.NumericFields = numericFields;
            end
        end
        
        function data = getField(obj,field)
            if ismember(field,obj.NumericFields)
                data = [obj.Data(:).(field)]';
            else
                data = {obj.Data(:).(field)}';
            end
        end
        
        function sobj = getSubset(obj,idx)
            idx = unique(idx);
            sobj = obj;
            sobj.Data = sobj.Data(idx);
            sobj.NumberOfEntries = numel(idx);
        end
        
        function str = getStructArray(obj)
            str = obj.Data;
        end
        
        function str = getIndexedStructArray(obj,idx)
            str = obj.Data(idx);
        end
        
    end
    
end