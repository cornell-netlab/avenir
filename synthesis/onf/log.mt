class_id := 0#32; if ordered
	ipv6_dst#128 = 42541956123771093561836879591493372040#128 -> class_id := 1008#32 []
	true -> skip []
fi; if ordered
	class_id#32 = 1017#32 -> out_port := 17#9 []
	class_id#32 = 1017#32 -> out_port := 18#9 []
	class_id#32 = 1010#32 -> out_port := 10#9 []
	class_id#32 = 1012#32 -> out_port := 12#9 []
	class_id#32 = 1011#32 -> out_port := 11#9 []
	class_id#32 = 1008#32 -> out_port := 8#9 []
	class_id#32 = 1003#32 -> out_port := 3#9 []
	class_id#32 = 1013#32 -> out_port := 13#9 []
	true -> out_port := 0#9 []
fi; if ordered
	ipv6_hop_count#8 <= 1#8 -> out_port := 0#9 []
	true -> ipv6_hop_count := (ipv6_hop_count#8 - 1#8) []
fi; if ordered
	out_port#9 = 0#9 -> ipv6_dst := 0#128; ipv6_src := 0#128; ipv6_next_header := 0#8; ipv6_hop_count := 0#8 []
	true -> skip []
fi