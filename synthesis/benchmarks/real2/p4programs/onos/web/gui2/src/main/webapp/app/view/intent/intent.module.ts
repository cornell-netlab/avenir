/*
 * Copyright 2018-present Open Networking Foundation
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
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { IntentComponent } from './intent/intent.component';
import { Gui2FwLibModule } from 'gui2-fw-lib/public_api';
import { RouterModule } from '@angular/router';

@NgModule({
    imports: [
        CommonModule,
        Gui2FwLibModule,
        RouterModule.forChild([{path: '', component: IntentComponent}]),
    ],
    declarations: [IntentComponent],
    exports: [IntentComponent]
})
export class IntentModule { }
