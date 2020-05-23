/*
 * Copyright 2015-present Open Networking Foundation
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
 ONOS GUI -- Topology Panel Module.
 Defines functions for manipulating the summary, detail, and instance panels.
 */

(function () {
    'use strict';

    // injected refs
    var $log, $window, $rootScope, fs, ps, gs, flash, wss, bns, mast, ns;

    // function to be replaced by the localization bundle function
    var topoLion = function (x) {
        return '#tps#' + x + '#';
    };

    // constants
    var pCls = 'topo-p',
        idSum = 'topo-p-summary',
        idDet = 'topo-p-detail',
        panelOpts = {
            width: 260, // summary and detail panel width
        },
        sumMax = 226, // summary panel max height
        padTop = 16, // summary panel padding below masthead
        padding = 16, // panel internal padding
        padFudge = padTop + 2 * padding;

    // internal state
    var useDetails = true, // should we show details if we have 'em?
        haveDetails = false, // do we have details that we could show?
        sumFromTop, // summary panel distance from top of screen
        unbindWatch;

    // panels
    var summary, detail;

    // === -----------------------------------------------------
    // Panel API
    function createTopoPanel(id, opts) {
        var p = ps.createPanel(id, opts),
            pid = id,
            header, body, footer;
        p.classed(pCls, true);

        function panel() {
            return p;
        }

        function hAppend(x) {
            return header.append(x);
        }

        function bAppend(x) {
            return body.append(x);
        }

        function fAppend(x) {
            return footer.append(x);
        }

        function setup() {
            p.empty();

            p.append('div').classed('header', true);
            p.append('div').classed('body', true);
            p.append('div').classed('footer', true);

            header = p.el().select('.header');
            body = p.el().select('.body');
            footer = p.el().select('.footer');
        }

        function destroy() {
            ps.destroyPanel(pid);
        }

        // fromTop is how many pixels from the top of the page the panel is
        // max is the max height of the panel in pixels
        //    only adjusts if the body content would be 10px or larger
        function adjustHeight(fromTop, max) {
            var totalPHeight, avSpace,
                overflow = 0;

            if (!fromTop) {
                $log.warn('adjustHeight: height from top of page not given');
                return null;
            } else if (!body || !p) {
                // panel contents are not defined
                // this may happen when window is resizing but panel has
                //   been cleared or removed
                return null;
            }

            p.el().style('top', fromTop + 'px');
            p.el().style('height', null);
            body.style('height', null);

            totalPHeight = fromTop + p.height();
            avSpace = fs.windowSize(padFudge).height;

            if (totalPHeight >= avSpace) {
                overflow = totalPHeight - avSpace;
            }

            function _adjustBody(height) {
                if (height < 10) {
                    return false;
                } else {
                    body.style('height', height + 'px');
                }
                return true;
            }

            if (!_adjustBody(fs.noPxStyle(body, 'height') - overflow)) {
                return p.height();
            }

            if (max && p.height() > max) {
                _adjustBody(fs.noPxStyle(body, 'height') - (p.height() - max));
            }
            return p.height();
        }

        return {
            panel: panel,
            setup: setup,
            destroy: destroy,
            appendHeader: hAppend,
            appendBody: bAppend,
            appendFooter: fAppend,
            adjustHeight: adjustHeight,
        };
    }

    // === -----------------------------------------------------
    // Utility functions

    function addSep(tbody) {
        tbody.append('tr').append('td').attr('colspan', 2).append('hr');
    }

    function addBtnFooter(sepAlreadyThere) {
        if (!sepAlreadyThere) {
            detail.appendFooter('hr');
        }
        detail.appendFooter('div').classed('actionBtns', true);
    }

    function addProp(tbody, label, value) {
        var tr = tbody.append('tr'),
            lab;
        if (typeof label === 'string') {
            lab = label.replace(/_/g, ' ');
        } else {
            lab = label;
        }

        function addCell(cls, txt) {
            tr.append('td').attr('class', cls).text(txt);
        }

        addCell('label', lab + ' :');
        addCell('value', value);
    }

    function listProps(tbody, data) {
        var sepLast = false;

        // note: track whether we end with a separator or not...
        data.propOrder.forEach(function (p) {
            if (p === '-') {
                addSep(tbody);
                sepLast = true;
            } else {
                addProp(tbody, data.propLabels[p], data.propValues[p]);
                sepLast = false;
            }
        });
        return sepLast;
    }

    function watchWindow() {
        unbindWatch = $rootScope.$watchCollection(
            function () {
                return {
                    h: $window.innerHeight,
                    w: $window.innerWidth,
                };
            }, function () {
                var h = summary.adjustHeight(sumFromTop, sumMax),
                    ss = summary.panel().isVisible(),
                    dtop = h && ss ? sumFromTop + h + padFudge : 0,
                    dy = dtop || ss ? detail.ypos.current : sumFromTop;
                detail.adjustHeight(dy);
            }
        );
    }

    // === -----------------------------------------------------
    //  Functions for populating the summary panel

    function populateSummary(data) {
        summary.setup();

        var svg = summary.appendHeader('div')
                .classed('icon', true)
                .append('svg'),
            title = summary.appendHeader('h2'),
            table = summary.appendBody('table'),
            tbody = table.append('tbody'),
            glyphId = data.glyphId || 'bird';

        gs.addGlyph(svg, glyphId, 24, 0, [1, 1]);

        title.text(data.title);
        listProps(tbody, data);

        augmentDetailPanel();
    }

    // === -----------------------------------------------------
    //  Functions for populating the detail panel

    var navPathIdKey = {
        device: 'devId',
        host: 'hostId',
    };

    function displaySingle(data) {
        var sepLast;

        detail.setup();

        var svg = detail.appendHeader('div')
                .classed('icon clickable', true)
                .append('svg'),
            title = detail.appendHeader('h2')
                .classed('clickable', true),
            table = detail.appendBody('table'),
            tbody = table.append('tbody'),
            navFn,
            navPath;

        gs.addGlyph(svg, (data.glyphId || 'm_unknown'), 26);
        title.text(data.title);

        // add navigation hot-link if defined
        navPath = data.navPath;
        if (navPath) {
            navFn = function () {
                var arg = {};
                arg[navPathIdKey[navPath]] = data.id;
                ns.navTo(navPath, arg);
            };

            svg.on('click', navFn);
            title.on('click', navFn);
        }

        sepLast = listProps(tbody, data);
        addBtnFooter(sepLast);
    }

    function displayMulti(ids) {
        detail.setup();

        var title = detail.appendHeader('h3'),
            table = detail.appendBody('table'),
            tbody = table.append('tbody');

        title.text(topoLion('title_selected_items'));
        ids.forEach(function (d, i) {
            addProp(tbody, i + 1, d);
        });
        addBtnFooter();
    }

    function addAction(o) {
        var btnDiv = d3.select('#' + idDet)
            .select('.actionBtns')
            .append('div')
            .classed('actionBtn', true);
        bns.button(btnDiv, idDet + '-' + o.id, o.gid, o.cb, o.tt);
    }

    function displayNothing() {
        haveDetails = false;
        hideDetailPanel();
    }

    function displaySomething() {
        haveDetails = true;
        if (useDetails) {
            showDetailPanel();
        }
    }

    // === -----------------------------------------------------
    //  Event Handlers

    function showSummary(data) {
        populateSummary(data);
        showSummaryPanel();
    }

    function toggleSummary(x) {
        var kev = (x === 'keyev'),
            on = kev ? !summary.panel().isVisible() : !!x,
            verb = on ? topoLion('show') : topoLion('hide'),
            sumpan = topoLion('fl_panel_summary');

        if (on) {
            // ask server to start sending summary data.
            wss.sendEvent('requestSummary');
            // note: the summary panel will appear, once data arrives
        } else {
            hideSummaryPanel();
        }
        flash.flash(verb + ' ' + sumpan);
        return on;
    }

    // === -----------------------------------------------------
    // === LOGIC For showing/hiding summary and detail panels...

    function showSummaryPanel() {
        function _show() {
            summary.panel().show();
            summary.adjustHeight(sumFromTop, sumMax);
        }

        if (detail.panel().isVisible()) {
            detail.down(_show);
        } else {
            _show();
        }
    }

    function hideSummaryPanel() {
        // instruct server to stop sending summary data
        wss.sendEvent('cancelSummary');
        summary.panel().hide(detail.up);
    }

    function showDetailPanel() {
        if (summary.panel().isVisible()) {
            detail.down(detail.panel().show);
        } else {
            detail.up(detail.panel().show);
        }
    }

    function summaryBBox() {
        return d3.select('#' + idSum).node().getBoundingClientRect();
    }

    function hideDetailPanel() {
        detail.panel().hide();
    }

    // ==========================

    function augmentDetailPanel() {
        var d = detail,
            downPos = summaryBBox().bottom + padTop;

        d.ypos = { up: sumFromTop, down: downPos, current: downPos };

        d._move = function (y, cb) {
            var yp = d.ypos,
                endCb;

            if (fs.isF(cb)) {
                endCb = function () {
                    cb();
                    d.adjustHeight(d.ypos.current);
                };
            } else {
                endCb = function () {
                    d.adjustHeight(d.ypos.current);
                };
            }
            if (yp.current !== y) {
                yp.current = y;
                d.panel().el().transition().duration(300)
                    .each('end', endCb)
                    .style('top', yp.current + 'px');
            } else {
                endCb();
            }
        };

        d.down = function (cb) { d._move(d.ypos.down, cb); };
        d.up = function (cb) { d._move(d.ypos.up, cb); };
    }

    function toggleUseDetailsFlag(x) {
        var kev = (x === 'keyev'),
            verb;

        useDetails = kev ? !useDetails : !!x;
        verb = topoLion(useDetails ? 'enable' : 'disable');

        if (useDetails) {
            if (haveDetails) {
                showDetailPanel();
            }
        } else {
            hideDetailPanel();
        }
        flash.flash(verb + ' ' + topoLion('fl_panel_details'));
        return useDetails;
    }

    // ==========================

    function initPanels() {
        sumFromTop = mast.mastHeight() + padTop;
        summary = createTopoPanel(idSum, panelOpts);
        detail = createTopoPanel(idDet, panelOpts);

        augmentDetailPanel();
        watchWindow();
    }

    function destroyPanels() {
        summary.destroy();
        summary = null;

        detail.destroy();
        detail = null;
        haveDetails = false;
        unbindWatch();
    }

    // ==========================

    angular.module('ovTopo')
    .factory('TopoPanelService',
        ['$log', '$window', '$rootScope', 'FnService', 'PanelService', 'GlyphService',
            'FlashService', 'WebSocketService', 'ButtonService', 'MastService',
            'NavService',

        function (_$log_, _$window_, _$rootScope_,
                  _fs_, _ps_, _gs_, _flash_, _wss_, _bns_, _mast_, _ns_) {
            $log = _$log_;
            $window = _$window_;
            $rootScope = _$rootScope_;
            fs = _fs_;
            ps = _ps_;
            gs = _gs_;
            flash = _flash_;
            wss = _wss_;
            bns = _bns_;
            mast = _mast_;
            ns = _ns_;

            return {
                initPanels: initPanels,
                destroyPanels: destroyPanels,
                createTopoPanel: createTopoPanel,

                showSummary: showSummary,
                toggleSummary: toggleSummary,
                hideSummary: hideSummaryPanel,

                toggleUseDetailsFlag: toggleUseDetailsFlag,
                displaySingle: displaySingle,
                displayMulti: displayMulti,
                displayNothing: displayNothing,
                displaySomething: displaySomething,
                addAction: addAction,

                detailVisible: function () { return detail.panel().isVisible(); },
                summaryVisible: function () { return summary.panel().isVisible(); },

                setLionBundle: function (bundle) { topoLion = bundle; },
            };
        }]);
}());
