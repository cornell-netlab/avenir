apply(l3_fwd
     , (ipv4_dst : lpm
       , ipv4_src : selector
       , ipv4_proto : selector)
     , ( { \ (port) ->
                standard_metadata.egress_spec = port }
       | { \ () -> skip }
       | { \ () -> mark_to_drop(standard_metadata) } )
     , skip)
  
