/// Time-sensitive data with an expiration time
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TimedData<D> {
    /// Data
    pub data: D,

    /// Deadline or expiration time, using monotonic SAME symbol counter
    pub deadline: u64,
}

impl<D> TimedData<D>
where
    D: Clone + PartialEq + Eq,
{
    /// Store `data` with the given `deadline`
    pub fn with_deadline(data: D, deadline: u64) -> Self {
        TimedData { data, deadline }
    }

    /// Check for expiration
    pub fn is_expired_at(&self, now: u64) -> bool {
        self.deadline <= now
    }
}

impl<D> AsRef<D> for TimedData<D>
where
    D: Clone + PartialEq + Eq,
{
    fn as_ref(&self) -> &D {
        &self.data
    }
}
