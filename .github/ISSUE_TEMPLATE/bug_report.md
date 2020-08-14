---
name: Bug Report
about: If something is wrong, then we will make it right!
title: ''
labels: 'Bug Report'
assignees: ''

---

Thank you for filling out this bug report (BR). Please perform every step and answer every question to the best of your ability. That will make it easier for you and more accessible for us to address the issue with the highest level of quality in the shortest amount of time.

### Description

[Single sentence description of the bug.]

### Prerequisites

* [ ] Did you study the [README](https://github.com/org2blog/org2blog/blob/master/README.org) to learn how the feature you are reporting a bug is expected to work?
* [ ] Did you go through the [When Things Go Wrong](https://github.com/org2blog/org2blog#when-things-go-wrong-or-could-go-even-better) process? It covers the steps to review many possibilies
* [ ] Did you review the issue tracker to see if this bug is already
reported or resolved?
* [ ] Did you consider that what you are describing may be a feature request? If you answered yes, review both the issue tracker and the [FUTURE](https://github.com/org2blog/org2blog/blob/master/FUTURE.org)  file to see if the feature request already exists.

### Environment

- What operating system are you running Emacs on? [e.g., macOS 10.13 or Windows 10]
- How did you install Emacs? [e.g., compiled it, download and manual install, package manager like Apt on Linux or brew on macOS]
- Do you run Emacs in the GUI or a Terminal? [e.g., GUI or Terminal]
- What is the value of `org2blog/wp-blog-alist`? Here is how to add the value to this BR: Create an empty source block by inserting three backticks on a blank line, hit return to create a new empty line, then insert three backticks. Now the goal is to place the value on that empty line. One way is for you to copy your configuration into this block manually. It is the easiest. Another way is to ask Emacs to find the value so you can paste it onto this line. Here is how: hit `M-S-:`, type `(kill-new (with-output-to-string (princ org2blog/wp-blog-alist)))` and strike `[enter]`, the information is now in the `kill-ring`, go back to that empty line, and paste the value. You will have to format it now, but you will have the correct values for your system. Either way, remove any private information like passwords or internal URLs. It will look something like this markup and code:'

```
(("yourblog"
   :url "https://www.yourblog.com/xmlrpc.php"
   :username "you"
   :password "onlyyouknow"))
```

Now obtain critical library information by:
1. Hitting `M-x`
2. Typing in `org2blog-version-info`
3. Striking `[enter]`
4. The information appears in the minibuffer then gets added to the `kill-ring`, aka your clipboard.
5. Paste that message below:

```
Paste the message here.
```

### Server

- What is the URL of your blog?
- Do you have a web browser to use for testing?
- Can you reach that URL using that browser?
- Can you log into your blog using that browser?
- Can you create or edit a post using that browser?

### Minimal Reproducible Example

Read about creating a [Minimal Reproducible Example](https://stackoverflow.com/help/minimal-reproducible-example) to learn about the best way to write a BR. Although the post is tailored for their discussion board, the techniques and strategies apply perfectly to report a bug. After reading about their approach a couple of times, please move forward with the BR.

### Steps to Reproduce

1. [First Step]
2. [Second step]
3. [and so on...]

### Expected behavior

[What you expected to happen]

### Actual behavior

[What actually happened]

### Screenshots

If applicable, add screenshots to help explain your problem.

### Additional context

Add any other context about the problem here.
