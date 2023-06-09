# Contributing to Convert’s JavaScript SDK

We welcome contributions and feedback! Please read our [Code of Conduct](CODE_OF_CONDUCT.md) to keep our community approachable and respectable.

## Getting Started

Please check the [README](README.md) to set up your development environment, then read the guidelines below for information on submitting your code.

## Development Process

1. Fork the repository and create your branch from `main`.
2. Please follow the [Conventional Commits](https://www.conventionalcommits.org/) for each commit message.
3. Make sure to add tests.
4. Run `yarn lint` to ensure there are no lint errors.
5. `git push` your changes to GitHub.
6. Open a PR from your fork into the `main` branch of the original repo.
7. Make sure that all unit tests are passing and that there are no merge conflicts between your branch and `main`.
8. Open a pull request from `YOUR_NAME/branch_name` to `main`.
9. A repository maintainer will review your pull request and merge it once approved.

## Pull request acceptance criteria

- Test coverage threshold is 85%. We use Mocha's chai assertion library. Changes in functionality should have accompanying unit tests. Bug fixes should have accompanying regression tests.
- Tests are located in the `tests` directory.
- Please don't change the `package.json` or `CHANGELOG.md`. We'll take care of bumping the version when we next release.
- Lint your code with our `yarn lint` before submitting.

## Style

To enforce style rules, we use ESLint. See our [.eslintrc.js](.eslintrc.js) for more information on our specific style rules. As well as our [.prettierrc.js](.prettierrc.js) for more information on our code formatting rules.

## License

All contributions are under the CLA mentioned above. For this project, Convert Insights uses the Apache 2.0 license, and so asks that by contributing your code, you agree to license your contribution under the terms of the [Apache License v2.0](http://www.apache.org/licenses/LICENSE-2.0). Your contributions should also include the following header:

```
/****************************************************************************
 * Copyright YEAR, Convert Insights, Inc. and contributors                        *
 *                                                                          *
 * Licensed under the Apache License, Version 2.0 (the "License");          *
 * you may not use this file except in compliance with the License.         *
 * You may obtain a copy of the License at                                  *
 *                                                                          *
 *    http://www.apache.org/licenses/LICENSE-2.0                            *
 *                                                                          *
 * Unless required by applicable law or agreed to in writing, software      *
 * distributed under the License is distributed on an "AS IS" BASIS,        *
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. *
 * See the License for the specific language governing permissions and      *
 * limitations under the License.                                           *
 ***************************************************************************/
```

The YEAR above should be the year of the contribution. If work on the file has been done over multiple years, list each year in the section above. Example: Convert Insights writes the file and releases it in 2014. No changes are made in 2015. Change made in 2016. YEAR should be “2014, 2016”.

## Contact

If you have questions, please contact development@convert.com.
