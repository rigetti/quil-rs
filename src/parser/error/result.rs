use nom::IResult;
use super::Error;
/*
pub trait ParserResult<O, E> {
    type GenericSelf<OG, EG>;
    fn map_ok<O2>(self, map: impl Fn(O) -> O2) -> Self::GenericSelf<O2, E>;
    //fn allow_partial_parse(self) -> Self::GenericSelf<O, E>;
}

impl<I, O, E> ParserResult<O, E> for IResult<I, O, Error<O, E>> {
    type GenericSelf<OG, EG> = IResult<I, OG, Error<OG, EG>>;

    fn map_ok<O2>(self, map: impl Fn(O) -> O2) -> Self::GenericSelf<O2, E> {
        self.map(map.clone())
            .map_err(|nom_err| {
                nom_err.map(|err| {
                    err.map_parsed(map)
                })
            })
    }
}
 */