## GH-API-CLI

A terminal tool for querying the github API.

### Globals

- `--help` displays a help message
- `--version` displays the version of the software you are on
- `--token={token}` github personal access token for higher rate limits & priv repo access (just use a VPN to avoid rate limits, don't sell your data, don't be a sheep)
- `--format={json|table}` output done in a pretty table form or a json form. Defaults to table.
- `--limit={number}` maximum number of items to fetch from the API (pagination cap). Defaults to 1000 by default for now.

### Users

```
gh-api-cli --user={username} --{data} [--constraints] [-options]
```

There are a lot of available data tags:

- `--total-stars` - lists the total number of stars the user has
- `--most-used-lang` - lists the most used languages by percentage
  - Options:
    - `-a` lists percentage of every single language (default)
    - `-x` lists the top 10 languages
    - `-1` lists the most used language
    - `-2` lists top 2
    - `-3` lists top 3
    - `-4` lists top 4
    - `-5` lists top 5
    - `-6` lists top 6
    - `-7` lists top 7
    - `-8` lists top 8
    - `-9` lists top 9
  - Constraints:
    - `--exclude={lang1}` excludes lang1
    - `--exclude={lang1,lang2,lang3,...}` excludes however many languages you put there
  - Sorting (Optional):
    - `--sort={field}` - `percentage` - this is on by default, `name` - alphabetical
    - `--order={asc|desc}` - Order direction, `desc` for down, `asc` for up. `desc` is default for `percentage` and `asc` is default for `name`.
- `--repos` - lists every repository
  - Options:
    - `-a` lists every single repo (default)
    - `-x` lists the top 10 repos
    - `-1` lists top repos
    - `-2` lists top 2
    - `-3` lists top 3
    - `-4` lists top 4
    - `-5` lists top 5
    - `-6` lists top 6
    - `-7` lists top 7
    - `-8` lists top 8
    - `-9` lists top 9
  - Constraints:
    - `--exclude-lang={lang}` excludes all repos that use the `lang` language
    - `--exclude-lang={lang1,lang2}` excludes all repos that use `lang1` or `lang2`
    - `--exclude-main-lang={lang}` excludes all repos whose main language is `lang`
    - `--exclude-xor-lang={lang,lang2}` excludes all repos that use `lang` xor `lang2`
  - Sorting:
    - `--sort={field}` - `name`, `created`, `updated`, `pushed`, `stars`, `forks`, `size`
    - `--order={asc|desc}` - Direction. `desc` is default for everything besides `name`. All of this should be very self-explanatory.
  - Filters:
    - `--filter-name-substr={pattern}` - include only repos whose name matches the given substring
    - `--filter-name={pattern}` - include only repos that match the regex.
    - `--filter-created-after={YYYY-MM-DD}` - include only repos created after this date
    - `--filter-created-before={YYYY-MM-DD}` - include only repos created before this date.
    - `--filter-updated-after={YYYY-MM-DD}` - include only repos last updated on or after this date.
    - `--filter-updated-before={YYYY-MM-DD}` - Include repos last updated on or before this date.
    - `--filter-updated-visibility={public|private|internal|all}` - include repos with given visibility. Default is `all`
    - `--filter-fork={true|false}` - include only forked repos or only non-forked repos. By default it'll show all.
    - `--filter-archived={true|false}` - include only archived (`true`) or only active (`false`) repos. By default it'll show all.
    - `--filter-language={lang}` - include repos that use the given language (any occurrence). Can be repeated or comma-separated for multiple languages (uses OR logic).
    - `--filter-main-language={lang}` - only includes repos whose main language is `lang`.
  - Caveats:
    - Exclude filters take precedence over include filters. If both `--exclude-lang` and `--filter-language` are given, exclude removes matching repos after inclusion.
- `--stars` - lists every starred repository
  - Options:
    - `-a` list all starred repos (default).
    - `-x` list top 10 starred repos.
    - `-1` through `-9` list top $N$ starred repos.
  - Sorting:
    - `--sort={field}` sort starred repos by:
      - `starred_at` the date the repo was starred (default, `desc`)
      - `name` repo name (`asc`)
      - `stars` number of stars (`desc`)
      - `updated` the `updated_at` property of the repo (`desc`)
      - `created` when the repo was created (`desc`)
    - `--order={asc|desc}` self explanatory
  - Filters:
- `--followers` - lists every follower
  - Options:
    - `-a` list all
    - `-x` list top 10
    - `-1` through `-9` you know what this does
  - Sorting:
    - `--sort={field}`, with fields of `name`, `joined`, `followers`, and `repos` (repo count).
    - `--order={asc|desc}`
  - Filters:
    - `--filter-name={pattern}` only those that match the regex
    - `--filter-name-substr={pattern}` only those whose username contains the substring
    - `--filter-company={pattern}` only followers whose company matches the substring
- `--following` - lists everyone the user follows
  - Identical to `--followers`, just lists people that the user follows instead
- `--orgs` - lists every organization the user is in
  - Identical to `--followers` but doesn't feature the filter by company option

### Organizations

- I will figure this out whenever I'm not tired
