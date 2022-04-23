module Domain.DailyWorkLog

/// A work log for a single day.
/// Use as DailyWorkLog or RawDailyWorkLog.
type GenericDailyWorkLog<'a> =
    { name: string
      scheduledHours: decimal
      committedHoursOtherDays: decimal
      workUnits: 'a list }

/// A single unit of work.
type WorkUnit = { hours: decimal; comment: string }

/// The work log of a single project on a single day.
type DailyWorkLog = GenericDailyWorkLog<WorkUnit>
