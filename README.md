# React-like Emacs Elisp Design

Aim to streamline the process of building model interactive flex layout app that resemble the react development experience.

## Get Started

### Building Dynamic Module
Assume you already have `rust` installed.
```sh
git clone https://github.com/knilink/reed.el.git
cd reed.el
cargo build --release
```
If success, `libreed.so` should be generated at `target/release/libreed.so`

### First App
Create a file `my-first-app.el` with the following content
```elisp
;; -*- lexical-binding: t -*-
(require 'reed-elx)
(require 'reed-hooks)
(require 'reed-style)
(require 'reed-render-helper)

(fc! Container (children)
     (elx!
      (div
       :style (style!*
               (size (width . 100%) (height . AUTO))
               (flex_direction . 'Column)
               (align_items . '(Center)))
       ({} children))))

(fc! Row (children)
     (elx!
      (div
       :style (style!*
               (justify_content . '(Center))
               (size (width . 100%) (height . AUTO)))
       ({} children))))

(fc! App ()
     (let ((current-time-sig (use-signal (lambda () (current-time-string))))
           (buffer-name-ref (use-ref (lambda () (buffer-name)))))
       (use-hook-with-cleanup
        (lambda ()
          (run-with-timer
           0 1
           (lambda ()
             (funcall current-time-sig (current-time-string))
             (reed-handle-render (funcall buffer-name-ref)))))
        (lambda (timer-handle) (cancel-timer timer-handle)))
       (elx!
        (Container
         (Row (p (span
                  :face '(:foreground "red" :background "yellow")
                  "Hello, ")
                 (span
                  :face '(:foreground "green" :background "blue")
                  "World!")))
         (Row (p "Time is now: " ({} (funcall current-time-sig))))))))

(reed-render-buffer "my-first-app" #'App)
```
Then run the app with
```sh
emacs -l reed.el/target/release/libreed.so -L reed.el/lisp -l ./my-first-app.el -Q
```

## Example App
- [oh-puhn-text-ui.el](https://github.com/knilink/oh-puhn-text-ui.el): A more interactive text ui llm frontend.

## Elements
### Tags
- **`div`**
   The fundamental layout container. Used for structuring UI components with flexible styling options.

- **`p`** (Paragraph)
   Text container that serves as a leaf node in the layout hierarchy.
   - Children can only be `span` elements
   - Primarily used for text content with styling

- **`span`**
   Inline text styling element.
   - Only valid as a child of `p` elements
   - Supports only the `face` property for text styling
   - Used for applying different styles to portions of text

### Attributes
- **`style`**
  S-expressions serialized [taffy](https://github.com/DioxusLabs/taffy) layout Style.

- **`face`**
  Face text property for styling text.

- **`ref`**
  Provides a way to obtain element id, subsequently can be use to retrieve element position etc. A ref is created with `use-ref` hook.

### Event Listeners
Event listeners will be triggered accordingly depends on event type and position attribute, event payload will be forwarded directly to listeners.
- **`onclick`**: Triggered when cursor event `click` is emitted and the position is inside the target element.
- **`onfocus`**: Triggered when click inside an element while the target element isn't focused.
- **`onblur`**: Triggered when click outside an element while the target element is focused.
- **`onhover`**: Triggered when cursor `move` is emitted and the position is inside the target element.
- **`onleave`**: Triggered when cursor `move` is emitted and the position is outside the target element.

To emit a cursor event:

```el
(reed-handle-cursor-event my-registered-app-name 'click my-event-position my-event-payload)
(reed-handle-cursor-event "my-first-app" 'move 123 '(:position 123))
```

## Elx Macros

### **elx!**
Provides a convenient way to create react element.
Tag starts with lower case will be treated as a native element tag otherwise a component.
Syntax: `(elx! (tag ,@attr-plist ,@children) (Component ,@props-plist ,@children) &rest)`

### **{}** Curly Brace Syntax
The body of `{}` will be treated as dynamic content and evaluated during runtime.
For example,
```elisp
(elx! (div ({} (if flag (elx! (p "true")) "false"))))
```
is equivalent to
```jsx
<div>{flag ? <p>true</p> : "false"}</div>
```

### **fc!** Functional Component
Used to define reusable React-like components that returns a element, use keyword symbol to define alias
Example:
```
(fc! MyComponent (foo :bar bar-alias)
  (message "[bar-alias] %s" bar-alias)
  (elx! ...))

(elx! (MyComponent :foo "foo-value" :bar "bar-value"))
```

## Style
S-expressions serialized [taffy](https://github.com/DioxusLabs/taffy) layout Style.

### Length
To create the 4 different type of serialized length attribute, use `reed-taffy-length`.
- percent: `(reed-taffy-length 'percent 0.5)`
- length: `(reed-taffy-length 'length 80.0)`
- auto: `(reed-taffy-length 'auto 0.0)`
- zero: `(reed-taffy-length 'zero 0.0)`

### Optional Attribute
When specifying attribute declared as `Optional` in rust, e.g. [`justify_content: Option<JustifyContent>`](https://github.com/DioxusLabs/taffy/blob/33adacc8083672045b89fc3b7e796b8ec95e258d/src/compute/flexbox.rs#L150). The value need to be wrapped with brackets `()` to represent `Some(attr)`, e.g. `(justify_content . '(Center))`.

### **style!** macro
Helper to compose style property, main to help expanding short handed length. Length value needs to be ended either `pt` or `%`, e.g.
```elisp
(style!
  (margin
    (left . 100pt)
    (right . 20%)
    (top . ZERO)
    (bottom . AUTO)))
```

### **style!\*** macro
Same as `style!` but style will be expanded into static string during macro expansion. Recommended if style is static throughout the entire component lifetime.

## Hooks

### **use-signal**
```
(fc! App ()
     (let ((count-sig (use-signal (lambda() 0))))
       (elx!
        (p
         "Count: "
         ({} (funcall count-sig)))
        (p " ")
        (p
         :face '(:background "gray80" :box (:line-width 1 :style released-button))
         :onclick (lambda (e)
                    (funcall count-sig (1+ (funcall count-sig))))
           "click me!"))))
```

### **use-effect**
```elisp
(fc! App ()
     (let ((element-ref (use-ref (lambda())))
           (location-sig (use-signal (lambda())))
           (buffer-name-ref (use-ref (lambda () (buffer-name)))))
       (use-effect
        (lambda ()
          (run-with-timer
           0 nil
           (lambda ()
             (unless (funcall location-sig)
               (funcall
                location-sig
                (reed-get-absolut-location
                 (funcall buffer-name-ref)
                 (funcall element-ref)))
               (reed-handle-render (funcall buffer-name-ref)))))))
       (elx!
        (div
         :style (style!*
                 (padding (left . ZERO) (right . ZERO) (top . 10pt) (bottom . ZERO))
                 (justify_content . '(Center))
                 (size (width . 100%) (height . AUTO)))
         (p
          :ref element-ref
          ({} (format "this element located at %s" (funcall location-sig))))))))
```
