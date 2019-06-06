(library
   (name ${c_api.lib_name})
   (public_name ${c_api.lib_name})
   (flags (-w -32-9))
   (libraries ctypes ctypes.foreign camomile))
