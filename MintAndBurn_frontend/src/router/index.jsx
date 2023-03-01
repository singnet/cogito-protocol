import { lazy, Suspense } from 'react';
import { Routes, Route } from 'react-router-dom';
import SnetLinearLoader from '../components/ui/snet-linear-loader';
import paths from './paths';

const Voting = lazy(() => import('../pages/Voting'));
const NotFound = lazy(() => import('../pages/NotFound'));

const routes = () => {
  return (
    <Suspense fallback={<SnetLinearLoader />}>
      <Routes>
        <Route exact path={paths.Voting} element={<Voting/>} />
        <Route path={paths.NotFound} element={<NotFound />} />
      </Routes>
    </Suspense>
  );
};

export default routes;
