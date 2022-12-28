use anyhow::{bail, Context};
use fnv::FnvHashMap as HashMap;

use crate::{parse::*, Date, Day, OkOrFail, Puzzle, Result};

const DATE: Date = Date::new(Day::D10, super::YEAR);
pub(super) const PUZZLE: Puzzle = Puzzle::new(DATE, solve);

#[allow(clippy::needless_pass_by_value)]
fn solve(input: String) -> Result {
    let instructions = parse_instructions(&input)?;
    let (mut bots, mut can_proceed) = Bots::init(instructions)?;
    if can_proceed.is_empty() {
        bail!("no bot was given two microchips");
    }
    let mut bot_17_61 = None;
    let mut outputs = HashMap::default();
    while let Some(bot_id) = can_proceed.pop() {
        let bot = bots.get_mut(bot_id)?;
        let (Some(val1), Some(val2)) = bot.values else {
            bail!("bot {} does not have two microchips", bot_id);
        };
        if (val1 == 17 && val2 == 61 || val1 == 61 && val2 == 17) && bot_17_61.is_none() {
            bot_17_61 = Some(bot_id);
        }
        bot.values = (None, None);
        let low_value = std::cmp::min(val1, val2);
        let high_value = std::cmp::max(val1, val2);
        let config = bot.config;
        macro_rules! proceed {
            ($next:expr, $value:expr) => {
                match $next {
                    Next::Bot(bot_id) => {
                        if bots.store(bot_id, $value)? {
                            can_proceed.push(bot_id);
                        }
                    }
                    Next::Output(output_id) => {
                        if outputs.insert(output_id, $value).is_some() {
                            bail!("output {} already has a microchip", output_id);
                        }
                    }
                }
            };
        }
        proceed!(config.next_low, low_value);
        proceed!(config.next_high, high_value);
    }
    let bot_17_61 = bot_17_61.ok_or_fail("could not find a bot comparing 17 and 61")?;
    let out0 = u32::from(*outputs.get(&0).ok_or_fail("there is no microchip in output 0")?);
    let out1 = u32::from(*outputs.get(&1).ok_or_fail("there is no microchip in output 1")?);
    let out2 = u32::from(*outputs.get(&2).ok_or_fail("there is no microchip in output 2")?);
    let out_product = out0 * out1 * out2;
    answer!(bot_17_61, out_product);
}

type BotId = u8;
type OutputId = u8;
type Value = u8;

#[derive(Copy, Clone)]
enum Next {
    Bot(BotId),
    Output(OutputId),
}

#[derive(Copy, Clone)]
struct BotConfig {
    next_low: Next,
    next_high: Next,
}

struct Bot {
    config: BotConfig,
    values: (Option<Value>, Option<Value>),
}

impl Bot {
    const fn new(config: BotConfig) -> Self {
        Self { config, values: (None, None) }
    }

    fn store(&mut self, value: Value) -> Result<bool> {
        match self.values {
            (None, None) => self.values.0 = Some(value),
            (Some(_), None) => {
                self.values.1 = Some(value);
                return Ok(true);
            }
            (None, Some(_)) => {
                self.values.0 = Some(value);
                return Ok(true);
            }
            (Some(_), Some(_)) => bail!("bot already has two microchips"),
        }
        Ok(false)
    }
}

#[derive(Default)]
struct Bots(HashMap<BotId, Bot>);

impl Bots {
    fn init(instructions: Vec<Instruction>) -> Result<(Self, Vec<BotId>)> {
        let mut inputs = Vec::new();
        let mut bots = Self::default();
        let mut can_proceed = Vec::new();
        for instruction in instructions {
            match instruction {
                Instruction::Input(bot_id, value) => inputs.push((bot_id, value)),
                Instruction::Bot(bot_id, bot_config) => {
                    bots.insert(bot_id, Bot::new(bot_config))?;
                }
            }
        }
        for (bot_id, value) in inputs {
            if bots.store(bot_id, value)? {
                can_proceed.push(bot_id);
            }
        }
        Ok((bots, can_proceed))
    }

    fn insert(&mut self, bot_id: BotId, bot: Bot) -> Result<()> {
        if self.0.insert(bot_id, bot).is_some() {
            bail!("bot {} already exists", bot_id);
        }
        Ok(())
    }

    fn get_mut(&mut self, bot_id: BotId) -> Result<&mut Bot> {
        match self.0.get_mut(&bot_id) {
            Some(bot) => Ok(bot),
            None => bail!("could not find bot {}", bot_id),
        }
    }

    fn store(&mut self, bot_id: BotId, value: Value) -> Result<bool> {
        self.get_mut(bot_id)?.store(value).with_context(|| format!("bot {bot_id}"))
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    Input(BotId, Value),
    Bot(BotId, BotConfig),
}

fn parse_instructions(s: &str) -> Result<Vec<Instruction>> {
    s.lines().map(parse_instruction).collect()
}

#[rustfmt::skip]
fn parse_instruction(s: &str) -> Result<Instruction> {
    named!(bot_id(Str<'_>) -> BotId, sep!(space0, do_parse!(
        tag!("bot") >> bot: uint!(BotId) >> (bot)
    )));
    named!(output_id(Str<'_>) -> OutputId, sep!(space0, do_parse!(
        tag!("output") >> output: uint!(OutputId) >> (output)
    )));
    named!(input(Str<'_>) -> Instruction, sep!(space0, do_parse!(
        tag!("value")   >> value: uint!(Value) >>
        tag!("goes to") >> bot: bot_id >>
        (Instruction::Input(bot, value))
    )));
    named!(next(Str<'_>) -> Next,
        alt!(map!(bot_id, Next::Bot) | map!(output_id, Next::Output)
    ));
    named!(bot_config(Str<'_>) -> Instruction, sep!(space0, do_parse!(
        bot:       bot_id >> tag!("gives low to") >>
        next_low:  next   >> tag!("and high to")  >>
        next_high: next   >>
        (Instruction::Bot(bot, BotConfig { next_low, next_high }))
    )));
    nom!(alt!(Str(s), bot_config | input))
}
