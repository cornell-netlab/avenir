/*
 * Copyright 2016-present Open Networking Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 ONOS GUI -- Topology Layout Module.
 Module that contains the d3.force.layout logic
 */

(function () {
    'use strict';

    // Injected refs
    var $log;

    // Internal State
    var proj;

    function projection(x) {
        if (x) {
            proj = x;
            $log.debug('Set the projection');
        }
        return proj;
    }

    angular.module('ovTopo2')
    .factory('Topo2MapConfigService',
        ['$log',
            function (_$log_) {

                $log = _$log_;

                return {
                    projection: projection,
                };
            },
        ]
    );
})();
