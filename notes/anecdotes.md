##

dual homing

if I see unknown source from unknown port, the packet will automatically get dropped.
-- not described in the spec
-- not aware of such a table
-- src mac coming in 
-- internal host table (mac, port)
  * updated with a bridging exact match
  * mac address located here
  * not located correctly in a specific series of events
    -- believes host is on 


top level: inconsistency between spec and implementation

middle: bug inside implementation



vlan tag and translate it or assign
-- ofdpa handles untagged packets weirdly -- every packet has to have a vlan tagged (even untagged)
it can then be popped. has to be internally assigned if 
-- table 10 have to push a vlan tag -- determined by configuations
-- port determines vlan
-- same vlan for bridging
-- requires 2 flow rules
  * vlan assignment rule : match vlan untagged and push vlan x
  * vlan filtering rule : match internally assigned vlan and move to next table
  

Order of flows is important
-- installation order is reversed from flow


termination mac (multicast/unicast)
-- some switch architectures ()
-- unicast entries have to be before multicast entries



Qumran
-- dont let you match on iport
-- Tomahawk/Trident2 forces you to match on input port
-- Since you cannot match on input port
-- onos controller has to store which mcast group is on which port

-- Doesn't expose the full capability you need
-- admit multicast and then it gets crazy
   * happens when you remove the flows
   * entries are shared!
   
-- 2 years between ONF & Broadcomm to solve these problems!
-- Write a test to prove the issue!
