# Q.U.E.S.T.I.O.N.S.

## Acronym Breakdown

**QUESTIONS** stands for:
> **Q**uick **U**ser **E**ngagement & **S**urvey **T**ool for **I**nformational
> and **O**utcome-based **N**avigation of (Selective) **S**ubjects

## Overview

**Questions** is a highly interactive OTP application written in Erlang that
facilitates dynamic user engagement, surveys, and informational navigation. It
allows users to explore structured decision paths through interactive
applications such as quizzes, surveys, and adventure games.

At its core, **Questions** enables rapid deployment of session-based
conversational applications that can:

- Conduct user surveys.
- Provide informational guidance.
- Drive user engagement in outcome-driven decision trees.

Each session is stateful, secure, and can persist user interactions and
decisions across time.

## Key Features

- ✅ **Session Management** with unique Session IDs.
- ✅ **State Persistence** for user interactions.
- ✅ **Interactive Applications** (surveys, games, guided flows).
- ✅ **Session Timeout Handling** for idle sessions.
- ✅ **Customizable Logic** for branching and decision-making.

## Getting Started

### Prerequisites

- Erlang/OTP 25 or newer (recommended OTP 27)
- rebar3 build tool

### Build Instructions

Clone the repository and build with `rebar3`:

```bash
$ git clone <repo-url>
$ cd questions
$ rebar3 compile
```

### Run in Erlang Shell

```bash
$ rebar3 shell
```

Inside the shell:

```erlang
% Launch an adventure game app
{ok, SessionId} = messaging:launch_app(adventure_game, intro).

% Interact with the app
messaging:interact(adventure_game, SessionId, "1").

% Show state or history
messaging:show_state(adventure_game, SessionId).
messaging:show_history(adventure_game, SessionId).
```

## Project Structure

The primary files with the requisite functionality can be found in the following
project structure:

```
/questions
  /src
    messaging.erl        % Core application logic
  /_build                % Build artifacts
  rebar.config           % Project configuration
```

## How It Works

- Every app is launched as an independent process, identified by an **AppName**
  and **SessionId**.
- Sessions are uniquely identified and validated to prevent unauthorized
  interactions.
- State and history are persisted to disk and restored on process restarts.
- Idle sessions automatically timeout and clean up after a configurable period.

## License

This project is licensed under the [Apache License 2.0](LICENSE.md).

Copyright 2025, Aniruddha Sen <thesilvernimbus@gmail.com>

You may obtain a copy of the license at:

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

## Author

Developed by **Aniruddha Sen** ([@TheSilverNimbus][1]) for **Precision
Development** ([@PrecisionDevelopment][2]).
  
[1]: https://github.com/TheSilverNimbus
  
[2]: https://github.com/PrecisionDevelopment
