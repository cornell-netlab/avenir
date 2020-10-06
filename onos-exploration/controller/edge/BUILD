COMPILE_DEPS = CORE_DEPS + JACKSON + [
    "@minimal_json//jar",
    "//drivers/default:onos-drivers-default",
    "//apps/hybrid/p4pipelines/sourcerouting:onos-apps-hybrid-p4pipelines-sourcerouting",
]

osgi_jar(
    deps = COMPILE_DEPS,
)

BUNDLES = [
    "//apps/hybrid/controller/edge:onos-apps-hybrid-controller-edge",
]

onos_app(
    app_name = "hybrid.controller.edge",
    category = "Traffic Engineering",
    description = "Programs only edge elements",
    included_bundles = BUNDLES,
    required_apps = [
        "hybrid.p4pipelines.sourcerouting",
    ],
    title = "Edge Controller",
    url = " ",
)
