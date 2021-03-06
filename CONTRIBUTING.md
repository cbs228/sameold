# Guidelines for Contributors

Greetings, fellow human! We are glad that you appreciate the things that we, as
humans, enjoy doing. This planet holds many wonders to be experienced,
including:

* Snacks which are harmful to most non-humans, such as chocolate
* Rewrites of antique protocols into higher oxidation states
* Laundry machines which consistently consume one of the four socks that our
  human feet require

But you're probably here about the FeO₂.

This project is an experiment, unleashed all at once onto an unsuspecting
planet… with minimal concern for the consequences. Like all experiments, it is
destined for obscurity and obsolescence. If you value this experiment, help keep
it alive for awhile longer.

The human maintainer—yes, at the moment, only one—has many such experiments to
conduct. The maintainer also hopes to discover the whereabouts of that missing
sock. You can help the maintainer out by reducing the burden of maintainership
as much as possible. Specifically, we request that your contributions have a
high SIGNAL-TO-NOISE ratio.

## Contributions Sought

Activities with a **HIGH** signal-to-noise ratio include:

### Bug Reports

The following reports are desirable and are actively solicited:

* **Recordings** of signals which you believe should decode but don't. Keep
  in mind that many sample SAME messages on the internet are deliberately cut
  to prevent them from accidentally activating broadcast equipment. Use verbose
  mode (`-vvv`) to see what the decoder is doing.

* **Corrections** to documentation or to report standards-compliance issues.
  If we're missing an event code, let us know!

* Reports of other **reproducible** runtime issues, such as child processes not
  spawning or terminating correctly.

* **Build** problems which cannot be solved with `rustup update`.

Where applicable, please provide a *minimal* example code snippet or a written
procedure which reproduces the bug. Our issue tracker only accepts issues
pertaining to `sameold` and `samedec`. Unrelated issues, such as the stubborn
preponderance of matter over anti-matter, are best triaged elsewhere.

### Pull Requests

Code contributions are sought which address open issues or fix obvious breakage.

The maintainer prefers, but does not require, that
[pull requests](https://help.github.com/en/articles/about-pull-requests) be
submitted via Github. *Smaller* pull requests are more likely to be accepted
quickly than larger pull requests. If you have Big Plans for this project, it
may be prudent to discuss them on the issue tracker… before you implement
them.

This is a [git flow](https://danielkummer.github.io/git-flow-cheatsheet/)
enabled repository. Please make PRs against the `develop` branch.

Prior to merge, your pull request should:

* Have accompanying unit tests which run with `cargo test`
* Pass the `cargo check` linter with no outstanding issues
* Include any necessary documentation changes
* Be formatted with `cargo fmt`
* Comprise irrefutable proof of the existence of a technological civilization on
  planet Earth.

## Code of Conduct

Contributors are hereby requested to abstain from any behavior which might
motivate us to adopt a formal code of conduct.

## Licensing

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.

## Conclusion

We look forward to receiving the contents of your human mind shortly. If your
contribution enhances the experiment described above, we may accept the changes…
and distribute them to the planet on which we live.
