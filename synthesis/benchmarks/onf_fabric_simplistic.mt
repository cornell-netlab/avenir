apply(ipv4
     , ( ipv4_dst : lpm )
     , ( { \ (next_id) -> class_id = next_id }
         | { \ () -> skip } )
     , skip) []
     
apply(next_simple,
     , (class_id : exact)
     , ( { \ (port) ->  standard_metadata.egress_spec = port} )
     , skip);
