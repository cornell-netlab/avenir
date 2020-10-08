all: source_routing

source_routing:
	p4c-bm2-ss --arch v1model -o source_routing_core.json --p4runtime-files source_routing_core_p4info.txt source_routing_core.p4
	p4c-bm2-ss --arch v1model -o source_routing_edge.json --p4runtime-files source_routing_edge_p4info.txt source_routing_edge.p4
