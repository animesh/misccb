function c = GetContig(contig, name)
% Get the contig from the struct array with name field 'name'
%
% INPUT
% contig  Struct array with contig properties
% name    Name of the contig
%
% OUTPUT
% c       Struct with only the contig with name field 'name'
% 
c = contig(arrayfun(@(x) isequal(x.name, name), contig));

end