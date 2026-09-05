# Security

## What Causality is exposed to

Causality is a single-file Shiny application with one optional network
client. Uploaded data reaches the app only as a CSV parsed into a data
frame for numeric computation; column values are never rendered back
as raw HTML. The app spawns no subprocess, and no path in the source
builds an R expression from user-supplied text.

## What the code does about it

Every statistic and every generated finding comes from the app's own
computation. The one network call is to a local model, off unless
Ollama is already running on this machine: it is fixed to
`http://127.0.0.1:11434`, carries a twenty-second timeout, and
receives only an already-finished explanatory sentence built from
statistics the app already computed, never the uploaded data itself.
The model cannot produce, change, or add a statistical value; a
missing or unreachable model is treated as a normal condition, and the
app falls back to the explanation it already generated. When a model
does answer, its text is HTML-escaped before display, with only line
breaks substituted afterward, so nothing it returns can inject markup
into the page.

## Reporting a problem

Open a private security advisory through the repository, or open a
normal issue if the problem is not sensitive. Please include the
version, what you did, and what you saw. There is no account and
nothing leaves this machine except to a local model you would have
had to set up yourself, so a report here is about the code rather
than about an incident.

## Scope

In scope: anything that causes uploaded data to be rendered as HTML
instead of used as data, that lets the local model's response affect
a computed statistic, or that reaches the local model's address from
anywhere other than the fixed loopback endpoint. Out of scope: Ollama
itself, which is reported to its own maintainers, and anything that
requires an attacker to already be running code on the same machine.
