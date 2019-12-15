import React from 'react';
import {
  Table as TableMui,
  TableHead,
  TableCell,
  TableRow,
  TableBody,
  Typography
} from '@material-ui/core';
import TeX from '@matejmazur/react-katex';
import { History, ResultStats } from '../utils/useRegression';
import { Input, location, dictionary } from '../data/Data';
import Tex from '@matejmazur/react-katex';

import * as tf from '@tensorflow/tfjs';
import { DataStats } from '../utils/createData';

type Props = {
  inputs: Input[];
  lastState: History;
  resultStats: ResultStats;
  dataStats: DataStats;
  title: string;
  dataLength: number;
  hideDetails: boolean;
  base: number;
};

const getP = (t: number) => {
  return tf
    .scalar(-Math.abs(t))
    .div(Math.sqrt(2))
    .erf()
    .add(1)
    .dataSync()[0];
};

const Table: React.FC<Props> = (props: Props) => {
  return (
    <>
      <b>
        {props.title} (N={props.dataLength})
      </b>
      <TableMui size="small">
        <TableHead>
          <TableRow style={{ backgroundColor: '#AAAAAA' }}>
            <TableCell style={{ width: '25%' }}>
              <b>Variable</b>
            </TableCell>
            <TableCell>
              <b>S.D. / Mean (Variable)</b>
            </TableCell>
            <TableCell colSpan={3}>
              <b>Estimate (Bus, Walk, Car)</b> (t-Statistic)
            </TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {props.hideDetails ? null : (
            <>
              <TableRow>
                <TableCell>(ASC)</TableCell>
                <TableCell>-</TableCell>
                {props.lastState.bias.map((item, index) => (
                  <>
                    {props.base === index ? <TableCell>-</TableCell> : null}
                    <TableCell>
                      <b>{item.toFixed(3)} </b>(
                      {props.resultStats.tValue[index].toFixed(3)}
                      {getP(props.resultStats.tValue[index]) < 0.05
                        ? '**'
                        : getP(props.resultStats.tValue[index]) < 0.1
                        ? '*'
                        : null}
                      )
                    </TableCell>
                  </>
                ))}
                {props.base === 2 ? <TableCell>-</TableCell> : null}
              </TableRow>

              {props.inputs.length === 0
                ? null
                : props.lastState.weights.map((item, index) => {
                    const key = props.inputs[index];
                    let row;

                    if (location[key].length === 3) {
                      row = (
                        <TableCell colSpan={3}>
                          <b>{item.toFixed(3)} </b>(
                          {props.resultStats.tValue[index + 2].toFixed(3)}
                          {getP(props.resultStats.tValue[index + 2]) < 0.05
                            ? '**'
                            : getP(props.resultStats.tValue[index + 2]) < 0.1
                            ? '*'
                            : null}
                          )
                        </TableCell>
                      );
                    } else {
                      row = Array(3).fill(<TableCell>-</TableCell>);
                      location[key].map(item2 => {
                        row[item2 - 1] = (
                          <TableCell>
                            <b>{item.toFixed(3)} </b>(
                            {props.resultStats.tValue[index + 2].toFixed(3)}
                            {getP(props.resultStats.tValue[index + 2]) < 0.05
                              ? '**'
                              : getP(props.resultStats.tValue[index + 2]) < 0.1
                              ? '*'
                              : null}
                            )
                          </TableCell>
                        );
                      });
                    }

                    return (
                      <TableRow>
                        <TableCell>{dictionary[key]}</TableCell>
                        <TableCell>
                          {props.dataStats[key].sd.toFixed(3)}
                          {' / '}
                          {props.dataStats[key].mean.toFixed(3)}
                        </TableCell>
                        {row}
                      </TableRow>
                    );
                  })}
            </>
          )}

          <TableRow style={{ backgroundColor: '#DDDDDD' }}>
            <TableCell rowSpan={2}>Statistics</TableCell>
            <TableCell>
              <Tex math={'\\mathcal{L}'}></Tex> {': '}-
              {props.lastState.loss.toFixed(3)}
            </TableCell>
            <TableCell>
              <Tex math={'\\mathcal{L}_{(EL)}'}></Tex>
              {': '}
              {(
                -Math.log(props.lastState.bias.length + 1) * props.dataLength
              ).toFixed(3)}
            </TableCell>
            <TableCell>
              <Tex math={'\\rho^2_{(EL)}_{EL}'}></Tex> {': '}
              {props.resultStats.rho.toFixed(3)}
            </TableCell>
            <TableCell>
              {'adj. '}
              <Tex math={'\\rho^2_{(EL)}_{EL}'}></Tex>
              {': '}
              {props.resultStats.rho2.toFixed(3)}
            </TableCell>
          </TableRow>
          <TableRow style={{ backgroundColor: '#DDDDDD' }}>
            <TableCell colSpan={5}>
              t-Statistic with ** / * indicates that it is significant with 95%
              / 90% confidential level
            </TableCell>
          </TableRow>
        </TableBody>
      </TableMui>
    </>
  );
};

export default Table;
