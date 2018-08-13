# Working with a team

Here are a few rules for working effectively in a team:

* Pull requests (PRs) should be reviewed. Having another pair of eyes looking
  at code changes is the best way to get rid of bugs due to inattention.
  Moreover the reviewer may suggest improvements which you can learn from.
  Finally it ensures that someone else can actually read and understand the
  code that you wrote.
* Make your PRs as focused/concise/small as possible. This
  helps avoid rebasing issues down the road. By clarifying the intent this will
  also allow the reviewer to provide a more meaningful review. Moreover, if the PR is large, the
  reviewer will not be able to keep all the code changes in mind. Finally if
  the PR takes too long to review the reviewer may not be able to be focus
  through the whole set of changes.
* Do not ask for a review until your code is working (CI passed)
  and actually ready for review (no "WIP", documentation-lacking PRs). On the
  one hand, it's rude and will waste the reviewer's time if they have to
  perform several review passes, and will have them context switch many times.
  On the other hand GitHub does not track what changes have already been
  reviewed, which can lead to someone reviewing the same snippet several times
  -- or worse, not reviewing it at all.
* Make sure that only one person is working on a particular problem at a time.
  Do this by making sure everyone knows when you start working on something and
  what your goal is. This can be done through assignments on tickets or by
  synchronizing on Slack/etc. This also avoids the extreme case: working on
  something that has been done already.
* If someone is blocked on a task that only you can fix to you, work on that
  task first.
* Push your code as soon as possible if other people depend on it.
