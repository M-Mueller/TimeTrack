module Domain.Misc

type ProjectName = string

/// Represents a single JIRA issue
type Issue = { key: string; title: string }
