use crate::rules::errors::Error;
use crate::rules::eval::LhsRhsPair;
use crate::rules::{self, path_value::*, TraversedTo};
use crate::rules::{CmpOperator, QueryResult, UnResolved};

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub(crate) enum UnaryResult<'r> {
    Success,
    Fail,
    SuccessWith(&'r PathAwareValue),
    FailWith(&'r PathAwareValue),
}

#[derive(Clone, Debug)]
pub(crate) struct QueryIn<'value> {
    pub(crate) diff: Vec<TraversedTo<'value>>,
    pub(crate) lhs: Vec<TraversedTo<'value>>,
    pub(crate) rhs: Vec<TraversedTo<'value>>,
}

impl<'value> QueryIn<'value> {
    fn new(
        diff: Vec<TraversedTo<'value>>,
        lhs: Vec<TraversedTo<'value>>,
        rhs: Vec<TraversedTo<'value>>,
    ) -> QueryIn<'value> {
        QueryIn { lhs, rhs, diff }
    }
}

#[derive(Clone, Debug)]
pub(crate) struct ListIn<'value> {
    pub(crate) diff: Vec<TraversedTo<'value>>,
    pub(crate) lhs: TraversedTo<'value>,
    pub(crate) rhs: TraversedTo<'value>,
}

impl<'value> ListIn<'value> {
    fn new(
        diff: Vec<TraversedTo<'value>>,
        lhs: TraversedTo<'value>,
        rhs: TraversedTo<'value>,
    ) -> ListIn<'value> {
        ListIn { lhs, rhs, diff }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Compare<'r> {
    Value(LhsRhsPair<'r>),
    QueryIn(QueryIn<'r>),
    ListIn(ListIn<'r>),
    ValueIn(LhsRhsPair<'r>),
}

#[derive(Clone, Debug)]
pub(crate) enum ComparisonResult<'r> {
    Success(Compare<'r>),
    Fail(Compare<'r>),
    NotComparable(NotComparable<'r>),
    RhsUnresolved(UnResolved<'r>, TraversedTo<'r>),
}

#[derive(Clone, Debug)]
pub(crate) enum ValueEvalResult<'value> {
    LhsUnresolved(UnResolved<'value>),
    UnaryResult(UnaryResult<'value>),
    ComparisonResult(ComparisonResult<'value>),
}

impl<'value> ValueEvalResult<'value> {
    pub(crate) fn fail<C>(self, c: C) -> ValueEvalResult<'value>
    where
        C: FnOnce(ValueEvalResult<'value>) -> ValueEvalResult<'value>,
    {
        if let ValueEvalResult::ComparisonResult(ComparisonResult::Success(_)) = &self {
            self
        } else {
            c(self)
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum EvalResult<'value> {
    Skip,
    Result(Vec<ValueEvalResult<'value>>),
}

#[derive(Clone, Debug)]
pub(crate) struct NotComparable<'r> {
    pub(crate) reason: String,
    pub(crate) pair: LhsRhsPair<'r>,
}

// pub(super) fn resolved<'value, E, R>(
//     qr: &QueryResult<'value>,
//     err: E,
// ) -> Result<&'value PathAwareValue, R>
// where
//     E: Fn(UnResolved<'value>) -> R,
// {
//     match qr {
//         QueryResult::Resolved(r) | QueryResult::Literal(r) => Ok(*r),
//         QueryResult::UnResolved(ur) => Err(err(ur.clone())),
//     }
// }

pub(crate) trait Comparator {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>>;
}

pub(crate) trait UnaryComparator {
    fn compare<'value>(
        &self,
        lhs: &[QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'value>>;
}

struct CommonOperator {
    comparator: fn(&PathAwareValue, &PathAwareValue) -> crate::rules::Result<bool>,
}

struct EqOperation {}
struct InOperation {}

fn selected<'query, 'value: 'query, U, R>(
    query_results: &'query [QueryResult<'value>],
    mut c: U,
    mut r: R,
) -> Vec<TraversedTo<'value>>
where
    U: FnMut(&UnResolved<'value>),
    R: FnMut(&mut Vec<TraversedTo<'value>>, TraversedTo<'value>),
{
    let mut aggregated = Vec::with_capacity(query_results.len());
    for each in query_results {
        match each {
            QueryResult::Literal(l) => r(&mut aggregated, rules::TraversedTo::Referenced(l)),
            QueryResult::Resolved(l) => r(&mut aggregated, l.clone()),
            QueryResult::UnResolved(ur) => c(ur),
            // QueryResult::Computed(l) => r(&mut aggregated, l),
        }
    }
    aggregated
}

fn flattened<'query, 'value, U>(
    query_results: &'query [QueryResult<'value>],
    c: U,
) -> Vec<TraversedTo<'value>>
where
    U: FnMut(&UnResolved<'value>),
{
    selected(query_results, c, |into, p| match p.clone_inner() {
        PathAwareValue::List((_, list)) => {
            into.extend(list.into_iter().map(TraversedTo::Owned).collect::<Vec<_>>());
        }

        rest => into.push(TraversedTo::Owned(rest)),
    })
}

impl Comparator for CommonOperator {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>> {
        let mut results = Vec::with_capacity(lhs.len());
        let lhs_flattened = flattened(lhs, |ur| {
            results.push(ValueEvalResult::LhsUnresolved(ur.clone()))
        });
        let rhs_flattened = flattened(rhs, |ur| {
            results.extend(lhs_flattened.iter().map(|lhs| {
                ValueEvalResult::ComparisonResult(ComparisonResult::RhsUnresolved(
                    ur.clone(),
                    lhs.clone(),
                ))
            }))
        });
        let rhs = &rhs_flattened;
        for each_lhs in lhs_flattened {
            for each_rhs in rhs {
                results.push(match_value(
                    each_lhs.clone(),
                    each_rhs.clone(),
                    self.comparator,
                ));
            }
        }
        Ok(EvalResult::Result(results))
    }
}

fn match_value<'value, C>(
    each_lhs: TraversedTo<'value>,
    each_rhs: TraversedTo<'value>,
    comparator: C,
) -> ValueEvalResult<'value>
where
    C: Fn(&PathAwareValue, &PathAwareValue) -> crate::rules::Result<bool>,
{
    match comparator(each_lhs.borrow_inner2(), each_rhs.borrow_inner2()) {
        Ok(cmp) => {
            if cmp {
                success(each_lhs, each_rhs)
            } else {
                fail(each_lhs, each_rhs)
            }
        }

        Err(Error::NotComparable(reason)) => {
            ValueEvalResult::ComparisonResult(ComparisonResult::NotComparable(NotComparable {
                reason,
                pair: LhsRhsPair {
                    lhs: each_lhs,
                    rhs: each_rhs,
                },
            }))
        }

        _ => unreachable!(),
    }
}

fn is_literal<'value>(query_results: &[QueryResult<'value>]) -> Option<&'value PathAwareValue> {
    if query_results.len() == 1 {
        if let QueryResult::Literal(p) = query_results[0] {
            return Some(p);
        }
    }
    None
}

fn string_in<'value>(
    lhs_value: TraversedTo<'value>,
    rhs_value: TraversedTo<'value>,
) -> ValueEvalResult<'value> {
    match (lhs_value.clone_inner(), rhs_value.clone_inner()) {
        (PathAwareValue::String((_, lhs)), PathAwareValue::String((_, rhs))) => {
            if rhs.contains(&lhs) {
                success(lhs_value, rhs_value)
            } else {
                fail(lhs_value, rhs_value)
            }
        }

        _ => not_comparable(lhs_value, rhs_value),
    }
}

fn not_comparable<'value>(
    lhs: TraversedTo<'value>,
    rhs: TraversedTo<'value>,
) -> ValueEvalResult<'value> {
    ValueEvalResult::ComparisonResult(ComparisonResult::NotComparable(NotComparable {
        pair: LhsRhsPair {
            lhs: lhs.clone(),
            rhs: rhs.clone(),
        },
        reason: format!("Type not comparable, {}, {}", lhs, rhs),
    }))
}

fn success<'value>(lhs: TraversedTo<'value>, rhs: TraversedTo<'value>) -> ValueEvalResult<'value> {
    ValueEvalResult::ComparisonResult(ComparisonResult::Success(Compare::Value(LhsRhsPair {
        lhs,
        rhs,
    })))
}

fn fail<'value>(lhs: TraversedTo<'value>, rhs: TraversedTo<'value>) -> ValueEvalResult<'value> {
    ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::Value(LhsRhsPair {
        lhs,
        rhs,
    })))
}

fn contained_in<'value>(
    lhs_value: TraversedTo<'value>,
    rhs_value: TraversedTo<'value>,
) -> ValueEvalResult<'value> {
    match lhs_value.clone_inner() {
        PathAwareValue::List((_, lhsl)) => match rhs_value.clone_inner() {
            PathAwareValue::List((_, rhsl)) => {
                if !rhsl.is_empty() && rhsl[0].is_list() {
                    if rhsl.contains(&lhs_value.clone_inner()) {
                        ValueEvalResult::ComparisonResult(ComparisonResult::Success(
                            Compare::ListIn(ListIn::new(vec![], lhs_value, rhs_value)),
                        ))
                    } else {
                        ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::ListIn(
                            ListIn::new(
                                vec![lhs_value.clone()],
                                lhs_value.clone(),
                                rhs_value.clone(),
                            ),
                        )))
                    }
                } else {
                    let diff = lhsl
                        .into_iter()
                        .filter(|each| !rhsl.contains(each))
                        .map(TraversedTo::Owned)
                        .collect::<Vec<_>>();
                    if diff.is_empty() {
                        ValueEvalResult::ComparisonResult(ComparisonResult::Success(
                            Compare::ListIn(ListIn::new(diff, lhs_value, rhs_value)),
                        ))
                    } else {
                        ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::ListIn(
                            ListIn::new(diff, lhs_value, rhs_value),
                        )))
                    }
                }
            }

            _ => {
                ValueEvalResult::ComparisonResult(ComparisonResult::NotComparable(NotComparable {
                    pair: LhsRhsPair {
                        lhs: lhs_value.clone(),
                        rhs: rhs_value.clone(),
                    },
                    reason: format!("Can not compare type {}, {}", lhs_value, rhs_value),
                }))
            }
        },

        rest => match rhs_value.clone_inner() {
            PathAwareValue::List((_, rhsl)) => {
                if rhsl.contains(&rest) {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Success(Compare::ValueIn(
                        LhsRhsPair::new(rules::TraversedTo::Owned(rest), rhs_value),
                    )))
                } else {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::ValueIn(
                        LhsRhsPair::new(rules::TraversedTo::Owned(rest), rhs_value),
                    )))
                }
            }

            rhs_rest => match_value(
                rules::TraversedTo::Owned(rest),
                rules::TraversedTo::Owned(rhs_rest),
                compare_eq,
            ),
        },
    }
}

impl Comparator for InOperation {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>> {
        let mut results = Vec::with_capacity(lhs.len());
        match (is_literal(lhs), is_literal(rhs)) {
            (Some(l), Some(r)) => {
                results.push(
                    string_in(
                        rules::TraversedTo::Referenced(l),
                        rules::TraversedTo::Referenced(r),
                    )
                    .fail(|_| {
                        contained_in(
                            rules::TraversedTo::Referenced(l),
                            rules::TraversedTo::Referenced(r),
                        )
                    }),
                );
            }

            (Some(l), None) => {
                let rhs = selected(
                    rhs,
                    |ur| {
                        results.push(ValueEvalResult::ComparisonResult(
                            ComparisonResult::RhsUnresolved(
                                ur.clone(),
                                rules::TraversedTo::Referenced(l),
                            ),
                        ))
                    },
                    Vec::push,
                );

                if rhs.iter().any(|elem| elem.is_list()) {
                    rhs.into_iter().for_each(|r| {
                        results.push(contained_in(rules::TraversedTo::Referenced(l), r))
                    });
                } else if let PathAwareValue::List((_, list)) = l {
                    let diff = list
                        .iter()
                        .map(TraversedTo::Referenced)
                        .filter(|elem| !rhs.contains(elem))
                        .collect::<Vec<_>>();

                    if diff.is_empty() {
                        results.push(ValueEvalResult::ComparisonResult(
                            ComparisonResult::Success(Compare::QueryIn(QueryIn {
                                diff,
                                rhs,
                                lhs: vec![rules::TraversedTo::Referenced(l)],
                            })),
                        ));
                    } else {
                        results.push(ValueEvalResult::ComparisonResult(ComparisonResult::Fail(
                            Compare::QueryIn(QueryIn {
                                diff,
                                rhs,
                                lhs: vec![rules::TraversedTo::Referenced(l)],
                            }),
                        )));
                    }
                } else {
                    rhs.iter().for_each(|rhs_elem| {
                        results.push(contained_in(
                            rules::TraversedTo::Referenced(l),
                            rhs_elem.clone(),
                        ))
                    });
                }
            }

            (None, Some(r)) => {
                selected(
                    lhs,
                    |ur| results.push(ValueEvalResult::LhsUnresolved(ur.clone())),
                    Vec::push,
                )
                .into_iter()
                .for_each(|l| match r {
                    PathAwareValue::String(_) => match l.borrow_inner() {
                        PathAwareValue::List((_, lhsl)) => {
                            for eachl in lhsl {
                                results.push(string_in(
                                    rules::TraversedTo::Referenced(eachl),
                                    rules::TraversedTo::Referenced(r),
                                ));
                            }
                        }

                        rest => results.push(string_in(
                            rules::TraversedTo::Referenced(rest),
                            rules::TraversedTo::Referenced(r),
                        )),
                    },

                    rest => results.push(contained_in(l, rules::TraversedTo::Referenced(rest))),
                });
            }

            (None, None) => {
                let lhs_selected = selected(
                    lhs,
                    |ur| results.push(ValueEvalResult::LhsUnresolved(ur.clone())),
                    Vec::push,
                );
                let rhs_selected = selected(
                    rhs,
                    |ur| {
                        results.extend(lhs_selected.iter().map(|lhs| {
                            ValueEvalResult::ComparisonResult(ComparisonResult::RhsUnresolved(
                                ur.clone(),
                                lhs.clone(),
                            ))
                        }))
                    },
                    Vec::push,
                );

                let mut diff = Vec::with_capacity(lhs_selected.len());
                'each_lhs: for eachl in &lhs_selected {
                    for eachr in &rhs_selected {
                        if let ValueEvalResult::ComparisonResult(ComparisonResult::Success(_)) =
                            contained_in(eachl.clone(), eachr.clone())
                        {
                            continue 'each_lhs;
                        }
                    }
                    diff.push(eachl.clone());
                }

                results.push(if diff.is_empty() {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Success(Compare::QueryIn(
                        QueryIn::new(diff, lhs_selected, rhs_selected),
                    )))
                } else {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::QueryIn(
                        QueryIn::new(diff, lhs_selected, rhs_selected),
                    )))
                });
            }
        }
        Ok(EvalResult::Result(results))
    }
}

impl Comparator for EqOperation {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>> {
        let mut results = Vec::with_capacity(lhs.len());
        match (is_literal(lhs), is_literal(rhs)) {
            (Some(l), Some(r)) => {
                results.push(match_value(
                    rules::TraversedTo::Referenced(l),
                    rules::TraversedTo::Referenced(r),
                    compare_eq,
                ));
            }

            (Some(l), None) => {
                let rhs = selected(
                    rhs,
                    |ur| {
                        results.push(ValueEvalResult::ComparisonResult(
                            ComparisonResult::RhsUnresolved(
                                ur.clone(),
                                rules::TraversedTo::Referenced(l),
                            ),
                        ))
                    },
                    Vec::push,
                );

                match l {
                    PathAwareValue::List(_) => {
                        for each in rhs {
                            results.push(match_value(
                                rules::TraversedTo::Referenced(l),
                                each,
                                compare_eq,
                            ));
                        }
                    }

                    single_value => {
                        for eachr in rhs {
                            match eachr.borrow_inner2() {
                                PathAwareValue::List((_, rhsl)) => {
                                    for each_rhs in rhsl {
                                        results.push(match_value(
                                            rules::TraversedTo::Owned(single_value.clone()),
                                            rules::TraversedTo::Owned(each_rhs.clone()),
                                            compare_eq,
                                        ));
                                    }
                                }

                                rest_rhs => {
                                    results.push(match_value(
                                        rules::TraversedTo::Owned(single_value.clone()),
                                        rules::TraversedTo::Owned(rest_rhs.clone()),
                                        compare_eq,
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            (None, Some(r)) => {
                let lhs_flattened = selected(
                    lhs,
                    |ur| results.push(ValueEvalResult::LhsUnresolved(ur.clone())),
                    Vec::push,
                );
                match r {
                    PathAwareValue::List((_, rhsl)) => {
                        for each in lhs_flattened {
                            if each.is_scalar() && rhsl.len() == 1 {
                                results.push(match_value(
                                    each,
                                    rules::TraversedTo::Referenced(&rhsl[0]),
                                    compare_eq,
                                ))
                            } else {
                                results.push(match_value(
                                    each,
                                    rules::TraversedTo::Referenced(r),
                                    compare_eq,
                                ));
                            }
                        }
                    }

                    single_value => {
                        for each in lhs_flattened {
                            if let PathAwareValue::List((_, lhs_list)) = each.borrow_inner2() {
                                for each_lhs in lhs_list {
                                    results.push(match_value(
                                        rules::TraversedTo::Owned(each_lhs.clone()),
                                        rules::TraversedTo::Owned(single_value.clone()),
                                        compare_eq,
                                    ));
                                }
                            } else {
                                results.push(match_value(
                                    each.clone(),
                                    rules::TraversedTo::Owned(r.clone()),
                                    compare_eq,
                                ));
                            }
                        }
                    }
                }
            }

            (None, None) => {
                let lhs_selected = selected(
                    lhs,
                    |ur| results.push(ValueEvalResult::LhsUnresolved(ur.clone())),
                    Vec::push,
                );
                let rhs_selected = selected(
                    rhs,
                    |ur| {
                        results.extend(lhs_selected.iter().map(|lhs| {
                            ValueEvalResult::ComparisonResult(ComparisonResult::RhsUnresolved(
                                ur.clone(),
                                rules::TraversedTo::Owned(lhs.clone_inner()),
                            ))
                        }))
                    },
                    Vec::push,
                );

                let diff = if lhs_selected.len() > rhs_selected.len() {
                    lhs_selected
                        .iter()
                        .filter(|e| !rhs_selected.contains(*e))
                        .cloned()
                        .collect::<Vec<_>>()
                } else {
                    rhs_selected
                        .iter()
                        .filter(|e| !lhs_selected.contains(*e))
                        .cloned()
                        .collect::<Vec<_>>()
                };

                results.push(if diff.is_empty() {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Success(Compare::QueryIn(
                        QueryIn::new(diff, lhs_selected, rhs_selected),
                    )))
                } else {
                    ValueEvalResult::ComparisonResult(ComparisonResult::Fail(Compare::QueryIn(
                        QueryIn::new(diff, lhs_selected, rhs_selected),
                    )))
                });
            }
        }
        Ok(EvalResult::Result(results))
    }
}

impl Comparator for CmpOperator {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>> {
        if lhs.is_empty() || rhs.is_empty() {
            return Ok(EvalResult::Skip);
        }

        match self {
            CmpOperator::Eq => EqOperation {}.compare(lhs, rhs),
            CmpOperator::In => InOperation {}.compare(lhs, rhs),
            CmpOperator::Lt => CommonOperator {
                comparator: compare_lt,
            }
            .compare(lhs, rhs),
            CmpOperator::Gt => CommonOperator {
                comparator: compare_gt,
            }
            .compare(lhs, rhs),
            CmpOperator::Le => CommonOperator {
                comparator: compare_le,
            }
            .compare(lhs, rhs),
            CmpOperator::Ge => CommonOperator {
                comparator: compare_ge,
            }
            .compare(lhs, rhs),
            _ => Err(Error::IncompatibleError(format!(
                "Operation {} NOT PERMITTED",
                self
            ))),
        }
    }
}

impl Comparator for (CmpOperator, bool) {
    fn compare<'query, 'value: 'query>(
        &self,
        lhs: &'query [QueryResult<'value>],
        rhs: &'query [QueryResult<'value>],
    ) -> crate::rules::Result<EvalResult<'query>> {
        let results = self.0.compare(lhs, rhs)?;
        Ok(match results {
            EvalResult::Skip => EvalResult::Skip,
            EvalResult::Result(r) => {
                if self.1 {
                    EvalResult::Result(
                        r.into_iter()
                            .map(|e| match e {
                                ValueEvalResult::ComparisonResult(ComparisonResult::Fail(c)) => {
                                    match c {
                                        Compare::QueryIn(qin) => {
                                            let mut reverse_diff =
                                                Vec::with_capacity(qin.lhs.len());
                                            for each in &qin.lhs {
                                                if !qin.diff.contains(each) {
                                                    reverse_diff.push(each.clone())
                                                }
                                            }
                                            if reverse_diff.is_empty() {
                                                ValueEvalResult::ComparisonResult(
                                                    ComparisonResult::Success(Compare::QueryIn(
                                                        QueryIn::new(
                                                            reverse_diff,
                                                            qin.lhs,
                                                            qin.rhs,
                                                        ),
                                                    )),
                                                )
                                            } else {
                                                ValueEvalResult::ComparisonResult(
                                                    ComparisonResult::Fail(Compare::QueryIn(
                                                        QueryIn::new(
                                                            reverse_diff,
                                                            qin.lhs,
                                                            qin.rhs,
                                                        ),
                                                    )),
                                                )
                                            }
                                        }

                                        Compare::ListIn(lin) => {
                                            let lhs = match lin.lhs.borrow_inner2() {
                                                PathAwareValue::List((_, v)) => v,
                                                _ => unreachable!(),
                                            };
                                            let mut reverse_diff = Vec::with_capacity(lhs.len());
                                            for each in lhs {
                                                let each = TraversedTo::Owned(each.clone());
                                                if !lin.diff.contains(&each) {
                                                    reverse_diff.push(each)
                                                }
                                            }
                                            if reverse_diff.is_empty() {
                                                ValueEvalResult::ComparisonResult(
                                                    ComparisonResult::Success(Compare::ListIn(
                                                        ListIn::new(
                                                            reverse_diff,
                                                            lin.lhs.clone(),
                                                            lin.rhs,
                                                        ),
                                                    )),
                                                )
                                            } else {
                                                ValueEvalResult::ComparisonResult(
                                                    ComparisonResult::Fail(Compare::ListIn(
                                                        ListIn::new(
                                                            reverse_diff,
                                                            lin.lhs.clone(),
                                                            lin.rhs,
                                                        ),
                                                    )),
                                                )
                                            }
                                        }
                                        rest => ValueEvalResult::ComparisonResult(
                                            ComparisonResult::Success(rest),
                                        ),
                                    }
                                }

                                ValueEvalResult::ComparisonResult(ComparisonResult::Success(c)) => {
                                    match c {
                                        Compare::QueryIn(qin) => {
                                            let mut reverse_diff =
                                                Vec::with_capacity(qin.lhs.len());
                                            reverse_diff.extend(qin.lhs.clone());
                                            ValueEvalResult::ComparisonResult(
                                                ComparisonResult::Fail(Compare::QueryIn(
                                                    QueryIn::new(reverse_diff, qin.lhs, qin.rhs),
                                                )),
                                            )
                                        }

                                        Compare::ListIn(lin) => {
                                            let lhs = match lin.lhs.borrow_inner2() {
                                                PathAwareValue::List((_, v)) => v,
                                                _ => unreachable!(),
                                            };
                                            let mut reverse_diff = Vec::with_capacity(lhs.len());
                                            for each in lhs {
                                                reverse_diff.push(TraversedTo::Owned(each.clone()));
                                            }
                                            ValueEvalResult::ComparisonResult(
                                                ComparisonResult::Fail(Compare::ListIn(
                                                    ListIn::new(
                                                        reverse_diff,
                                                        lin.lhs.clone(),
                                                        lin.rhs.clone(),
                                                    ),
                                                )),
                                            )
                                        }

                                        rest => ValueEvalResult::ComparisonResult(
                                            ComparisonResult::Fail(rest),
                                        ),
                                    }
                                }

                                //
                                // Everything else
                                //
                                rest => rest,
                            })
                            .collect(),
                    )
                } else {
                    EvalResult::Result(r)
                }
            }
        })
    }
}

#[cfg(test)]
#[path = "operators_tests.rs"]
mod operators_tests;
