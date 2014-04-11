print("Success")

i = 0
local ffi = require("ffi")
ffi.cdef[[
typedef struct { int i, j; } mydata;
void callme(mydata *info);
]]
-- Note that the FORTRAN callme function needs to have a DDLEXPORT call, and
-- this needs to set the exported name to callme. It will then be available
-- through the ffi.C interface as shown below. The signature of callme needs
-- to be defined in a cdef call as above.

i = i + 1
print(i)
local info = ffi.new("mydata")
info.i = 10
info.j = 20
ffi.C.callme(info)
i = i + 1
print(i)
