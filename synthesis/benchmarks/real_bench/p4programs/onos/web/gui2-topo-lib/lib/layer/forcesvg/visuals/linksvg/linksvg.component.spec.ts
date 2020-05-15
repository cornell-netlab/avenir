/*
 * Copyright 2018-present Open Networking Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { LinkSvgComponent } from './linksvg.component';
import {LogService} from 'gui2-fw-lib/public_api';
import {ActivatedRoute, Params} from '@angular/router';
import {of} from 'rxjs';
import {Device, Link, RegionLink, LinkType} from '../../models';
import {BrowserAnimationsModule} from '@angular/platform-browser/animations';

class MockActivatedRoute extends ActivatedRoute {
    constructor(params: Params) {
        super();
        this.queryParams = of(params);
    }
}

describe('LinkVisualComponent', () => {
    let logServiceSpy: jasmine.SpyObj<LogService>;
    let component: LinkSvgComponent;
    let fixture: ComponentFixture<LinkSvgComponent>;
    let ar: MockActivatedRoute;
    let testLink: Link;
    let testDeviceA: Device;
    let testDeviceB: Device;

    beforeEach(async(() => {
        const logSpy = jasmine.createSpyObj('LogService', ['info', 'debug', 'warn', 'error']);
        ar = new MockActivatedRoute({ 'debug': 'txrx' });

        testDeviceA = new Device('test:A');
        testDeviceA.online = true;

        testDeviceB = new Device('test:B');
        testDeviceB.online = true;

        testLink = new RegionLink(LinkType.UiDeviceLink, testDeviceA, testDeviceB);
        testLink.id = 'test:A/1-test:B/1';

        TestBed.configureTestingModule({
            imports: [ BrowserAnimationsModule ],
            declarations: [ LinkSvgComponent ],
            providers: [
                { provide: LogService, useValue: logSpy },
                { provide: ActivatedRoute, useValue: ar },
            ]
        })
        .compileComponents();
        logServiceSpy = TestBed.get(LogService);
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(LinkSvgComponent);
        component = fixture.componentInstance;
        component.link = testLink;
        fixture.detectChanges();
    });

    it('should create', () => {
        expect(component).toBeTruthy();
    });
});
