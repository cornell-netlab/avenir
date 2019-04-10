COMPILE_DEPS = CORE_DEPS + [
    "@minimal_json//jar",
    "//protocols/p4runtime/model:onos-protocols-p4runtime-model",
    "//protocols/p4runtime/api:onos-protocols-p4runtime-api",
    "//drivers/default:onos-drivers-default",
]

osgi_jar(
    deps = COMPILE_DEPS,
)

BUNDLES = [
    "//apps/hybrid/p4pipelines/srcmac:onos-apps-hybrid-p4pipelines-srcmac",
]

onos_app(
    app_name = "hybrid.p4pipelines.srcmac",
    category = "Pipeconf",
    description = "Provides pipeconf ",
    included_bundles = BUNDLES,
    required_apps = [
        "org.onosproject.drivers.p4runtime",
    ],
    title = "P4 Tutorial Pipeconf",
    url = "http://onosproject.org",
)
