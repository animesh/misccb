classdef CellAnnotationDataAdapter <  bioinfoservices.AnnotationDataAdapter
    
    properties
        FieldBinnig
    end
    
    methods
        
        function obj = CellAnnotationDataAdapter(data,fields,searchableFields,efficientFields,fieldBinning)
            obj.FieldNames = fields;
            obj.Data = data;
            obj.NumberOfEntries = numel(data{1});
            obj.EfficientAccessFields = efficientFields;
            obj.StringSearchableFields = searchableFields;
            if nargin==5
                obj.FieldBinnig = containers.Map(fields,fieldBinning);
            else
                obj.FieldBinnig = containers.Map(fields,1:numel(fields));
            end
        end
        
        function data = getField(obj,field)
            data = obj.Data{obj.FieldBinnig(field)};
        end
                
        function sobj = getSubset(obj,idx)
            idx = unique(idx);
            sobj = obj;
            for i = 1:numel(sobj.Data)
                sobj.Data{i} = sobj.Data{i}(idx);
            end
            sobj.NumberOfEntries = numel(idx);
        end
        
        function data = getIndexedField(obj,idx,field)
            data = obj.Data{obj.FieldBinnig(field)}(idx(:));
        end
        
        function varargout = getIndexedFields(obj,idx,fields)
            for i = 1:numel(fields)
                varargout{i} = getIndexedField(obj,idx,fields{i});
            end
        end
        
        function str = getStructArray(obj)
            n = 2*numel(obj.FieldNames);
            data = cell(1,n);
            [data{1:2:n}] = obj.FieldNames{:};
            [data{2:2:n}] = getFields(obj,obj.FieldNames);
            h = find(cellfun(@(x) isnumeric(x),data));
            for i = 1:numel(h)
                data{h(i)} = num2cell(data{h(i)});
            end            
            str = struct(data{:});
        end        
        
        function str = getIndexedStructArray(obj,idx)
            n = 2*numel(obj.FieldNames);
            data = cell(1,n);
            [data{1:2:n}] = obj.FieldNames{:};
            
            [data{2:2:n}] = getIndexedFields(obj,idx,obj.FieldNames);
            h = find(cellfun(@(x) isnumeric(x),data));
            if ~isempty(idx)
                for i = 1:numel(h)
                    data{h(i)} = num2cell(data{h(i)});
                end
            end
            str = struct(data{:});
        end
        
    end
end