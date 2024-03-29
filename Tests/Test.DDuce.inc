{
  Copyright (C) 2013-2022 Tim Sinaeve tim.sinaeve@gmail.com

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

{.$DEFINE TESTINSIGHT}
//NOTE : Memory leak tracking does not work very well at the moment, as it's
//reporting leaks when logging information during tests (calls to .Status etc).
{.$DEFINE LEAKCHECK}  // enables LeakCheck support
{.$DEFINE USE_FASTMM4_LEAK_MONITOR} // enables FastMM4 Memory Leak Tracking.

{.$DEFINE NULLABLE} // enables tests on nullable members
