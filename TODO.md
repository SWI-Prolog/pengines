# Pengine demo site

* Prolog Shell
  - Resize: allow changing widths of sub-windows.                           [OK]
  - Separate answer blocks (query-enter/answer inside output window?)       [OK]
  - New examples show up only after reload
  - Complete rebound I/O (also in examples).				    [OK]
  - Clearer feedback that we are waiting for input
  - Make messages work.							    [OK]
  - Make time/1 work.							    [OK]

  - Redesign:
    - Modularize into
      - Editor
      - Interactor (output, query selection)
        - Have a one-line query select/input and [run] with
          hidden/popup (preferences) output window.
    - Allow adding SWISH to a tutorial similar to this:

      ==
      <div class="swish">
      <pre class="code">
      p(X) :- q(X).
      </pre>
      <ul class="examples">
      <li>p(a).
      </ul>
      </div>
      ==

* Scratchpad:
  - Make clearer that `Scratchpad` is a link back home.
    - Have pengine icon in the top-left					   [OK]
  - Show current example
  - Make it clearer that you can make your own changes and version
  - Examples
    - Client Side Templating: Does not work with `ask-in-create'.
  - Wordnet application and demos.

* Admin
  - Remove irrelevant settings (e.g., listing).                            [OK]

# Editor

  - Ace or CodeMirror
    - Can be replaced; both seem to work with TogetherJS.

# TogetherJS

  - Further hooks to improve syncing?

# Tutorial pages with embedded SWISH
