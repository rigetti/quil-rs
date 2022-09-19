// Copyright 2022 Rigetti Computing
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use nom::error::ErrorKind;

/// A parsing error that has not been converted to something more user-friendly.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
#[error("internal parsing error: {0:?}")]
pub struct InternalParseError(ErrorKind);

impl InternalParseError {
    pub(crate) fn new(kind: ErrorKind) -> Self {
        Self(kind)
    }
}
